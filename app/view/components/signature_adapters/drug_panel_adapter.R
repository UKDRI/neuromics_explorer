# drug-specific: data loading, focal-view plot builders

drug_panel_adapter <- list(
  entity_name   = "Drug",                     # used in UI labels: "Selected: N drugs"
  embed_data    = function() readRDS("embed_drug.rds"),
  long_data     = function() readRDS("de_long_drug.rds"),
  focal_single  = function(id, entity, d) {    # 1-item focal view builder
    tagList(
      plotlyOutput(NS(id, "volcano")),
      plotlyOutput(NS(id, "heatmap_single"))
    )
  },
  focal_multi   = function(id, entities, d) {  # multi-item focal view builder
    tagList(
      plotlyOutput(NS(id, "heatmap_multi")),
      plotlyOutput(NS(id, "upset_plot"))
    )
  }
)