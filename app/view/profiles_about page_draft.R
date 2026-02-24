# About Page


profile_card_ui <- function(id, name, role, img, bio) {
  ns <- NS(id)
  
  card(
    class = "text-center",
    card_body(
      tags$img(
        src = img,
        style = "width:120px; height:120px; border-radius:50%; margin-bottom:10px;"
      ),
      tags$h5(name),
      tags$p(class = "text-muted", role),
      tags$p(bio)
    )
  )
}
# about_page_ui <- function(id) {
#   ns <- NS(id)
#   
#   tagList(
#     fluidRow(
#       column(
#         width = 8,
#         offset = 2,
#         tags$h2("About This Application", class = "text-center mb-4"),
#         tags$p("This application is designed to provide users with an intuitive interface for data analysis and visualization. It leverages the power of R and Shiny to deliver dynamic and interactive content."),
#         tags$p("Our team is dedicated to creating user-friendly tools that help individuals and organizations make informed decisions based on data insights.")
#       )
#     ),
#     fluidRow(
#       column(
#         width = 4,
#         profile_card_ui(
#           id = ns("member1"),
#           name = "Alice Smith",
#           role = "Data Scientist",
#           img = "www/alice.jpg",
#           bio = "Alice specializes in machine learning and statistical modeling. She has a passion for turning data into actionable insights."
#         )
#       ),
#       column(
#         width = 4,
#         profile_card_ui(
#           id = ns("member2"),
#           name = "Bob Johnson",
#           role = "Software Engineer",
#           img = "www/bob.jpg",
#           bio = "Bob is an expert in web development and application architecture. He ensures that our applications are robust and scalable."
#         )
#       ),
#       column(
#         width = 4,
#         profile_card_ui(
#           id = ns("member3"),
#           name = "Carol Lee",
#           role = "UX Designer",
#           img = "www/carol.jpg",
#           bio = "Carol focuses on creating intuitive user experiences. She designs interfaces that are both functional and visually appealing."
#         )
#       )
#     )
#   )
# }

about_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "container mt-4",
    
    layout_columns(
      col_widths = c(4, 4, 4),
      
      profile_card_ui(
        ns("alice"),
        name = "Alice Smith",
        role = "Data Scientist",
        img = "www/alice.png",
        bio = "Specializes in statistical modeling and visualization."
      ),
      
      profile_card_ui(
        ns("bob"),
        name = "Bob Lee",
        role = "Backend Engineer",
        img = "www/bob.png",
        bio = "Builds scalable data pipelines and APIs."
      ),
      
      profile_card_ui(
        ns("carol"),
        name = "Carol Jones",
        role = "UX Designer",
        img = "www/carol.png",
        bio = "Focuses on intuitive and accessible interfaces."
      )
    )
  )
}


# in main.R
nav_menu(
  title = "About",
  icon = icon("info-circle"),
  
  nav_panel(
    title = "About Us",
    icon = icon("users"),
    about_ui("about")
  )
)





# OR FOR MORE ANIMATIONS /ICONS
profile_card_ui <- function(id) {
  ns <- NS(id)
  
  card(
    class = "profile-card text-center",
    card_body(
      uiOutput(ns("card"))
    )
  )
}

profile_card_server <- function(id, member) {
  moduleServer(id, function(input, output, session) {
    
    output$card <- renderUI({
      tagList(
        tags$img(
          src = member$img,
          class = "profile-img"
        ),
        tags$h5(member$name),
        tags$p(class = "text-muted", member$role),
        tags$p(member$bio),
        
        div(
          class = "social-icons",
          lapply(member$social, function(link) {
            tags$a(
              href = link$url,
              target = "_blank",
              icon(link$icon)
            )
          })
        )
      )
    })
  })
}


#OR w CSS (Hover Animation + Polish)
# Add this once (in tags$style() or www/styles.css):
.profile-card {
  transition: all 0.3s ease;
  border-radius: 12px;
}

.profile-card:hover {
  transform: translateY(-8px);
  box-shadow: 0 12px 25px rgba(0,0,0,0.15);
}

.profile-img {
  width: 120px;
  height: 120px;
  border-radius: 50%;
  margin-bottom: 10px;
}

.social-icons a {
  margin: 0 6px;
  font-size: 1.2rem;
  color: #667eea;
}

.social-icons a:hover {
  color: #4c51bf;
}

