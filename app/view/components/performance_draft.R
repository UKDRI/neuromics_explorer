# These reactive and scalable system optimisation functions derive from datanovia; https://www.datanovia.com/learn/tools/shiny-apps/best-practices/accessibility-performance.html

# Comprehensive performance optimization framework
create_performance_optimization_framework <- function() {
  
  # Reactive caching strategies
  implement_smart_caching <- function() {
    # Multi-level caching system
    cache_manager <- list(
      # Level 1: Session-level cache (fastest)
      session_cache = reactiveValues(),
      
      # Level 2: Application-level cache (shared across sessions)
      app_cache = list(),
      
      # Level 3: Persistent cache (survives app restarts)
      persistent_cache_dir = "cache/"
    )
    
    # Smart caching wrapper for expensive operations
    cached_reactive <- function(expr, cache_key, cache_level = "session", 
                                expiry_minutes = 60, invalidate_on = NULL) {
      reactive({
        # Generate cache key
        full_key <- paste0(cache_key, "_", digest::digest(list(invalidate_on), algo = "md5"))
        
        # Check cache based on level
        cached_value <- switch(cache_level,
                               "session" = cache_manager$session_cache[[full_key]],
                               "app" = cache_manager$app_cache[[full_key]],
                               "persistent" = load_from_persistent_cache(full_key)
        )
        
        # Check if cache is valid
        if (!is.null(cached_value) && is_cache_valid(cached_value, expiry_minutes)) {
          logger::log_debug("Cache hit for key: {full_key}")
          return(cached_value$data)
        }
        
        # Cache miss - compute value
        logger::log_debug("Cache miss for key: {full_key} - computing...")
        start_time <- Sys.time()
        
        result <- expr
        
        computation_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        logger::log_info("Computed {full_key} in {computation_time} seconds")
        
        # Store in cache
        cache_entry <- list(
          data = result,
          timestamp = Sys.time(),
          computation_time = computation_time
        )
        
        switch(cache_level,
               "session" = {
                 cache_manager$session_cache[[full_key]] <- cache_entry
               },
               "app" = {
                 cache_manager$app_cache[[full_key]] <- cache_entry
               },
               "persistent" = {
                 save_to_persistent_cache(full_key, cache_entry)
               }
        )
        
        result
      }) %>% bindCache(invalidate_on)
    }
    
    # Cache invalidation utilities
    invalidate_cache <- function(pattern = NULL, cache_level = "all") {
      if (cache_level %in% c("session", "all")) {
        if (is.null(pattern)) {
          cache_manager$session_cache <- reactiveValues()
        } else {
          keys_to_remove <- grep(pattern, names(cache_manager$session_cache), value = TRUE)
          for (key in keys_to_remove) {
            cache_manager$session_cache[[key]] <- NULL
          }
        }
      }
      
      if (cache_level %in% c("app", "all")) {
        if (is.null(pattern)) {
          cache_manager$app_cache <- list()
        } else {
          keys_to_remove <- grep(pattern, names(cache_manager$app_cache), value = TRUE)
          for (key in keys_to_remove) {
            cache_manager$app_cache[[key]] <- NULL
          }
        }
      }
      
      if (cache_level %in% c("persistent", "all")) {
        clear_persistent_cache(pattern)
      }
    }
    
    # Cache statistics
    get_cache_stats <- function() {
      list(
        session_cache_size = length(cache_manager$session_cache),
        app_cache_size = length(cache_manager$app_cache),
        persistent_cache_size = count_persistent_cache_files(),
        cache_hit_rate = calculate_cache_hit_rate(),
        memory_usage = object.size(cache_manager)
      )
    }
    
    list(
      cached_reactive = cached_reactive,
      invalidate_cache = invalidate_cache,
      get_cache_stats = get_cache_stats
    )
  }
  
  # Asynchronous processing for long-running operations
  implement_async_processing <- function() {
    library(promises)
    library(future)
    
    # Configure future plan for parallel processing
    setup_async_backend <- function(workers = 4) {
      plan(multisession, workers = workers)
    }
    
    # Async wrapper for expensive computations
    async_reactive <- function(expr, session = getDefaultReactiveDomain()) {
      # Create reactive values for state management
      state <- reactiveValues(
        status = "idle",
        result = NULL,
        error = NULL,
        progress = 0
      )
      
      # Function to start async computation
      start_computation <- function() {
        state$status <- "running"
        state$error <- NULL
        state$progress <- 0
        
        # Create promise for async execution
        promise_obj <- future({
          expr
        }) %...>% (function(result) {
          state$result <- result
          state$status <- "completed"
          state$progress <- 100
          
          # Announce completion to screen readers
          if (!is.null(session)) {
            session$sendCustomMessage("announceUpdate", 
                                      "Computation completed successfully")
          }
          
          result
        }) %...!% (function(error) {
          state$error <- error$message
          state$status <- "error"
          
          logger::log_error("Async computation failed: {error$message}")
          
          # Announce error to screen readers
          if (!is.null(session)) {
            session$sendCustomMessage("announceUpdate", 
                                      paste("Computation failed:", error$message))
          }
          
          NULL
        })
        
        promise_obj
      }
      
      list(
        state = state,
        start = start_computation
      )
    }
    
    # Progress reporting for long operations
    create_progress_reporter <- function(session, total_steps) {
      progress <- shiny::Progress$new(session, min = 0, max = total_steps)
      progress$set(message = "Processing...", value = 0)
      
      # Update function
      update_progress <- function(step, message = NULL) {
        progress$set(value = step, message = message)
        
        # Update ARIA progress indicator
        percentage <- round((step / total_steps) * 100)
        session$sendCustomMessage("updateProgress", list(
          value = step,
          max = total_steps,
          percentage = percentage,
          message = message
        ))
      }
      
      # Cleanup function
      close_progress <- function() {
        progress$close()
      }
      
      list(
        update = update_progress,
        close = close_progress
      )
    }
    
    list(
      setup_async_backend = setup_async_backend,
      async_reactive = async_reactive,
      create_progress_reporter = create_progress_reporter
    )
  }
  
  # Data optimization techniques
  implement_data_optimization <- function() {
    # Efficient data loading with pagination
    paginated_data_loader <- function(data_source, page_size = 1000, filters = NULL) {
      reactive({
        # Apply filters first to reduce data size
        if (!is.null(filters) && length(filters) > 0) {
          # Implement server-side filtering for databases
          data_source <- apply_server_side_filters(data_source, filters)
        }
        
        # Get total count for pagination
        total_count <- get_data_count(data_source)
        
        # Load data in chunks
        load_data_chunk <- function(offset = 0, limit = page_size) {
          get_data_chunk(data_source, offset, limit)
        }
        
        list(
          loader = load_data_chunk,
          total_count = total_count,
          page_size = page_size
        )
      })
    }
    
    # Memory-efficient data processing
    process_large_dataset <- function(data, processing_func, chunk_size = 10000) {
      total_rows <- nrow(data)
      num_chunks <- ceiling(total_rows / chunk_size)
      
      results <- list()
      
      for (i in seq_len(num_chunks)) {
        start_row <- (i - 1) * chunk_size + 1
        end_row <- min(i * chunk_size, total_rows)
        
        chunk <- data[start_row:end_row, ]
        results[[i]] <- processing_func(chunk)
        
        # Force garbage collection after each chunk
        gc()
      }
      
      # Combine results
      do.call(rbind, results)
    }
    
    # Intelligent data sampling for previews
    create_smart_sample <- function(data, target_size = 1000, preserve_distribution = TRUE) {
      if (nrow(data) <= target_size) {
        return(data)
      }
      
      if (preserve_distribution) {
        # Stratified sampling to preserve data distribution
        categorical_cols <- sapply(data, function(x) is.factor(x) || is.character(x))
        
        if (any(categorical_cols)) {
          # Use first categorical column for stratification
          strata_col <- names(categorical_cols)[which(categorical_cols)[1]]
          
          # Sample proportionally from each stratum
          strata_counts <- table(data[[strata_col]])
          strata_props <- strata_counts / sum(strata_counts)
          strata_samples <- round(strata_props * target_size)
          
          sampled_indices <- c()
          for (stratum in names(strata_samples)) {
            stratum_indices <- which(data[[strata_col]] == stratum)
            sample_size <- min(strata_samples[[stratum]], length(stratum_indices))
            if (sample_size > 0) {
              sampled_indices <- c(sampled_indices, 
                                   sample(stratum_indices, sample_size))
            }
          }
          
          return(data[sampled_indices, ])
        }
      }
      
      # Simple random sampling
      sampled_indices <- sample(nrow(data), target_size)
      data[sampled_indices, ]
    }
    
    list(
      paginated_data_loader = paginated_data_loader,
      process_large_dataset = process_large_dataset,
      create_smart_sample = create_smart_sample
    )
  }
  
  # UI performance optimization
  implement_ui_optimization <- function() {
    # Lazy loading for UI components
    lazy_ui_component <- function(ui_function, trigger_condition) {
      conditionalPanel(
        condition = trigger_condition,
        ui_function()
      )
    }
    
    # Virtual scrolling for large lists
    create_virtual_scroll_list <- function(items, item_height = 50, visible_count = 20) {
      # JavaScript for virtual scrolling implementation
      virtual_scroll_js <- sprintf("
        $(document).ready(function() {
          var items = %s;
          var itemHeight = %d;
          var visibleCount = %d;
          var containerHeight = itemHeight * visibleCount;
          
          var container = $('#virtual-scroll-container');
          var viewport = $('<div>').css({
            height: containerHeight + 'px',
            overflow: 'auto',
            border: '1px solid #ccc'
          });
          
          var content = $('<div>').css({
            height: (items.length * itemHeight) + 'px',
            position: 'relative'
          });
          
          viewport.append(content);
          container.append(viewport);
          
          function renderVisibleItems(scrollTop) {
            var startIndex = Math.floor(scrollTop / itemHeight);
            var endIndex = Math.min(startIndex + visibleCount + 1, items.length);
            
            content.empty();
            
            for (var i = startIndex; i < endIndex; i++) {
              var item = $('<div>').css({
                position: 'absolute',
                top: (i * itemHeight) + 'px',
                height: itemHeight + 'px',
                width: '100%%',
                padding: '10px',
                borderBottom: '1px solid #eee'
              }).text(items[i]);
              
              content.append(item);
            }
          }
          
          viewport.on('scroll', function() {
            renderVisibleItems(this.scrollTop);
          });
          
          renderVisibleItems(0);
        });
      ", jsonlite::toJSON(items), item_height, visible_count)
      
      tagList(
        div(id = "virtual-scroll-container"),
        tags$script(HTML(virtual_scroll_js))
      )
    }
    
    # Debounced input handling
    create_debounced_input <- function(input_id, delay_ms = 300) {
      # JavaScript for input debouncing
      debounce_js <- sprintf("
        $(document).ready(function() {
          var timer;
          $('#%s').on('input', function() {
            var input = this;
            clearTimeout(timer);
            timer = setTimeout(function() {
              $(input).trigger('change');
            }, %d);
          });
        });
      ", input_id, delay_ms)
      
      tags$script(HTML(debounce_js))
    }
    
    list(
      lazy_ui_component = lazy_ui_component,
      create_virtual_scroll_list = create_virtual_scroll_list,
      create_debounced_input = create_debounced_input
    )
  }
  
  list(
    caching = implement_smart_caching(),
    async = implement_async_processing(),
    data = implement_data_optimization(),
    ui = implement_ui_optimization()
  )
}


# Automated comprehensive testing for both accessibility compliance and performance benchmarks
# Comprehensive testing framework for accessibility and performance
create_testing_framework <- function() {
  
  # Accessibility testing suite
  implement_accessibility_testing <- function() {
    library(testthat)
    library(shinytest2)
    
    # WCAG compliance tests
    test_wcag_compliance <- function(app_driver) {
      # Test color contrast
      test_that("Color contrast meets WCAG AA standards", {
        # Get computed styles
        contrast_results <- app_driver$get_js("
          var elements = document.querySelectorAll('*');
          var results = [];
          
          elements.forEach(function(el) {
            var style = window.getComputedStyle(el);
            var color = style.color;
            var backgroundColor = style.backgroundColor;
            
            if (color && backgroundColor && 
                color !== 'rgba(0, 0, 0, 0)' && 
                backgroundColor !== 'rgba(0, 0, 0, 0)') {
              results.push({
                element: el.tagName,
                color: color,
                backgroundColor: backgroundColor
              });
            }
          });
          
          return results;
        ")
        
        # Check each color combination
        for (result in contrast_results) {
          contrast_ratio <- calculate_contrast_from_rgb(result$color, result$backgroundColor)
          expect_gte(contrast_ratio, 4.5, 
                     info = paste("Element", result$element, "has insufficient contrast"))
        }
      })
      
      # Test keyboard navigation
      test_that("All interactive elements are keyboard accessible", {
        # Get all focusable elements
        focusable_elements <- app_driver$get_js("
          var focusable = document.querySelectorAll(
            'button, [href], input, select, textarea, [tabindex]:not([tabindex=\"-1\"])'
          );
          return Array.from(focusable).map(el => ({
            tagName: el.tagName,
            id: el.id,
            tabIndex: el.tabIndex,
            hasVisibleFocus: false  // Will be tested
          }));
        ")
        
        # Test each element for keyboard accessibility
        for (element in focusable_elements) {
          # Focus the element
          if (element$id != "") {
            app_driver$set_inputs(!!element$id := NULL)
            
            # Check if element has visible focus indicator
            has_focus <- app_driver$get_js(sprintf("
              var el = document.getElementById('%s');
              if (el) {
                el.focus();
                var style = window.getComputedStyle(el);
                return style.outline !== 'none' || 
                       style.boxShadow !== 'none' ||
                       el.classList.contains('focus');
              }
              return false;
            ", element$id))
            
            expect_true(has_focus, 
                        info = paste("Element", element$tagName, "lacks visible focus indicator"))
          }
        }
      })
      
      # Test semantic HTML structure
      test_that("Page has proper semantic structure", {
        # Check for landmarks
        landmarks <- app_driver$get_js("
          return {
            hasMain: document.querySelector('main, [role=\"main\"]') !== null,
            hasNav: document.querySelector('nav, [role=\"navigation\"]') !== null,
            headingCount: document.querySelectorAll('h1, h2, h3, h4, h5, h6').length,
            hasH1: document.querySelector('h1') !== null
          };
        ")
        
        expect_true(landmarks$hasMain, "Page should have a main landmark")
        expect_true(landmarks$hasH1, "Page should have an h1 heading")
        expect_gt(landmarks$headingCount, 0, "Page should have heading structure")
      })
      
      # Test alt text on images
      test_that("All images have appropriate alt text", {
        images_without_alt <- app_driver$get_js("
          var images = document.querySelectorAll('img');
          var withoutAlt = [];
          
          images.forEach(function(img) {
            if (!img.alt || img.alt.trim() === '') {
              withoutAlt.push({
                src: img.src,
                id: img.id || 'no-id'
              });
            }
          });
          
          return withoutAlt;
        ")
        
        expect_equal(length(images_without_alt), 0, 
                     info = paste("Images without alt text:", 
                                  paste(sapply(images_without_alt, function(x) x$src), collapse = ", ")))
      })
      
      # Test form accessibility
      test_that("All form inputs have proper labels", {
        unlabeled_inputs <- app_driver$get_js("
          var inputs = document.querySelectorAll('input, select, textarea');
          var unlabeled = [];
          
          inputs.forEach(function(input) {
            if (input.type === 'hidden') return;
            
            var id = input.id;
            var hasLabel = document.querySelector('label[for=\"' + id + '\"]') !== null;
            var hasAriaLabel = input.getAttribute('aria-label') || 
                              input.getAttribute('aria-labelledby');
            
            if (!hasLabel && !hasAriaLabel) {
              unlabeled.push({
                type: input.type || input.tagName,
                id: id || 'no-id'
              });
            }
          });
          
          return unlabeled;
        ")
        
        expect_equal(length(unlabeled_inputs), 0,
                     info = paste("Unlabeled inputs found:",
                                  paste(sapply(unlabeled_inputs, function(x) x$type), collapse = ", ")))
      })
    }
    
    # Screen reader simulation tests
    test_screen_reader_experience <- function(app_driver) {
      test_that("Content is properly announced to screen readers", {
        # Test ARIA live regions
        live_regions <- app_driver$get_js("
          return {
            statusRegions: document.querySelectorAll('[aria-live=\"polite\"]').length,
            alertRegions: document.querySelectorAll('[aria-live=\"assertive\"]').length,
            roles: Array.from(document.querySelectorAll('[role]')).map(el => el.getAttribute('role'))
          };
        ")
        
        expect_gt(live_regions$statusRegions + live_regions$alertRegions, 0,
                  "App should have ARIA live regions for dynamic content")
      })
      
      test_that("Tables have proper headers and structure", {
        table_structure <- app_driver$get_js("
          var tables = document.querySelectorAll('table');
          var results = [];
          
          tables.forEach(function(table) {
            var hasCaption = table.querySelector('caption') !== null;
            var hasHeaders = table.querySelectorAll('th').length > 0;
            var headerScopes = Array.from(table.querySelectorAll('th')).every(function(th) {
              return th.getAttribute('scope') !== null;
            });
            
            results.push({
              hasCaption: hasCaption,
              hasHeaders: hasHeaders,
              properScopes: headerScopes
            });
          });
          
          return results;
        ")
        
        for (table in table_structure) {
          expect_true(table$hasHeaders, "Tables should have header cells")
          expect_true(table$properScopes, "Table headers should have scope attributes")
        }
      })
    }
    
    list(
      test_wcag_compliance = test_wcag_compliance,
      test_screen_reader_experience = test_screen_reader_experience
    )
  }
  
  # Performance testing suite
  implement_performance_testing <- function() {
    library(shinyloadtest)
    library(microbenchmark)
    
    # Load testing with multiple users
    test_concurrent_users <- function(app_url, max_users = 10, duration_minutes = 5) {
      # Record user session
      recording_file <- "loadtest_recording.log"
      
      # Create recording if it doesn't exist
      if (!file.exists(recording_file)) {
        record_session(app_url, recording_file)
      }
      
      # Run load test with increasing user counts
      results <- list()
      
      for (user_count in c(1, 5, max_users)) {
        test_name <- paste0("users_", user_count)
        output_dir <- paste0("loadtest_", test_name)
        
        shinycannon(
          recording_file,
          app_url,
          workers = user_count,
          loaded_duration_minutes = duration_minutes,
          output_dir = output_dir
        )
        
        # Parse results
        results[[test_name]] <- parse_shinycannon_output(output_dir)
      }
      
      results
    }
    
    # Response time testing
    test_response_times <- function(app_driver) {
      test_that("Response times are within acceptable limits", {
        # Test initial page load
        start_time <- Sys.time()
        app_driver$wait_for_idle(timeout = 30000)
        initial_load_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        
        expect_lt(initial_load_time, 5, "Initial page load should be under 5 seconds")
        
        # Test input response times
        input_response_times <- list()
        
        # Test text input response
        start_time <- Sys.time()
        app_driver$set_inputs(test_input = "test value")
        app_driver$wait_for_idle()
        input_response_times$text_input <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        
        # Test button click response
        start_time <- Sys.time()
        app_driver$click("test_button")
        app_driver$wait_for_idle()
        input_response_times$button_click <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        
        # Verify all response times are reasonable
        for (interaction in names(input_response_times)) {
          expect_lt(input_response_times[[interaction]], 3,
                    info = paste(interaction, "response time should be under 3 seconds"))
        }
      })
    }
    
    # Memory usage testing
    test_memory_usage <- function(app_driver) {
      test_that("Memory usage remains stable", {
        initial_memory <- as.numeric(pryr::mem_used())
        
        # Perform multiple operations
        for (i in 1:10) {
          app_driver$set_inputs(test_data = runif(1000))
          app_driver$wait_for_idle()
          
          # Force garbage collection
          gc()
        }
        
        final_memory <- as.numeric(pryr::mem_used())
        memory_increase <- final_memory - initial_memory
        
        # Memory increase should be reasonable (less than 100MB)
        expect_lt(memory_increase, 100 * 1024^2, 
                  "Memory usage increase should be less than 100MB")
      })
    }
    
    # Database performance testing
    test_database_performance <- function(connection) {
      test_that("Database queries perform within acceptable limits", {
        # Test simple query
        simple_query_time <- microbenchmark(
          DBI::dbGetQuery(connection, "SELECT 1"),
          times = 10
        )
        
        expect_lt(median(simple_query_time$time) / 1e6, 100, # Convert to milliseconds
                  "Simple queries should complete under 100ms")
        
        # Test complex query (if applicable)
        complex_query_time <- microbenchmark(
          DBI::dbGetQuery(connection, "SELECT COUNT(*) FROM large_table WHERE condition = 'value'"),
          times = 5
        )
        
        expect_lt(median(complex_query_time$time) / 1e6, 5000, # 5 seconds
                  "Complex queries should complete under 5 seconds")
      })
    }
    
    # Benchmarking reactive expressions
    benchmark_reactive_performance <- function(reactive_expr, test_data) {
      # Benchmark reactive expression execution
      benchmark_results <- microbenchmark(
        reactive_expr(test_data$small),
        reactive_expr(test_data$medium),
        reactive_expr(test_data$large),
        times = 10
      )
      
      # Performance should scale reasonably with data size
      small_time <- median(benchmark_results$time[1:10])
      medium_time <- median(benchmark_results$time[11:20])
      large_time <- median(benchmark_results$time[21:30])
      
      # Check that performance scaling is reasonable (not exponential)
      scaling_factor <- large_time / small_time
      expect_lt(scaling_factor, 100, "Performance should scale reasonably with data size")
      
      list(
        small_data_time = small_time / 1e6,  # Convert to milliseconds
        medium_data_time = medium_time / 1e6,
        large_data_time = large_time / 1e6,
        scaling_factor = scaling_factor
      )
    }
    
    list(
      test_concurrent_users = test_concurrent_users,
      test_response_times = test_response_times,
      test_memory_usage = test_memory_usage,
      test_database_performance = test_database_performance,
      benchmark_reactive_performance = benchmark_reactive_performance
    )
  }
  
  # Automated testing pipeline
  create_testing_pipeline <- function() {
    # Comprehensive test suite runner
    run_full_test_suite <- function(app_url, database_connection = NULL) {
      results <- list()
      
      # Initialize app driver
      app_driver <- AppDriver$new(app_url)
      
      tryCatch({
        # Run accessibility tests
        results$accessibility <- run_accessibility_tests(app_driver)
        
        # Run performance tests
        results$performance <- run_performance_tests(app_driver)
        
        # Run database tests if connection provided
        if (!is.null(database_connection)) {
          results$database <- run_database_tests(database_connection)
        }
        
        # Generate comprehensive report
        results$summary <- generate_test_summary(results)
        
      }, finally = {
        app_driver$stop()
      })
      
      results
    }
    
    # CI/CD integration
    setup_ci_testing <- function() {
      # GitHub Actions workflow for testing
      github_workflow <- '
name: Accessibility and Performance Tests

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Set up R
      uses: r-lib/actions/setup-r@v2
      with:
        r-version: "4.3"
    
    - name: Install dependencies
      run: |
        Rscript -e "install.packages(c(\\"shiny\\", \\"testthat\\", \\"shinytest2\\"))"
    
    - name: Run accessibility tests
      run: |
        Rscript -e "source(\\"tests/test_accessibility.R\\")"
    
    - name: Run performance tests
      run: |
        Rscript -e "source(\\"tests/test_performance.R\\")"
    
    - name: Upload test results
      uses: actions/upload-artifact@v3
      with:
        name: test-results
        path: test-results/
      '
      
      # Write workflow file
      dir.create(".github/workflows", recursive = TRUE, showWarnings = FALSE)
      writeLines(github_workflow, ".github/workflows/accessibility-performance-tests.yml")
    }
    
    list(
      run_full_test_suite = run_full_test_suite,
      setup_ci_testing = setup_ci_testing
    )
  }
  
  list(
    accessibility = implement_accessibility_testing(),
    performance = implement_performance_testing(),
    pipeline = create_testing_pipeline()
  )
}
