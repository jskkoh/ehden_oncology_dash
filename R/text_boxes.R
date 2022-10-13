# Text for pop-up dialogue boxes 

modalDivs = function(){
  
  list( 
    # Data sources modal
    div(
      class="modal fade", id="sourcesModal", tabindex="-1", "aria-labelledby"="sourcesModalLabel",  "aria-hidden"="true",
      div(
        class="modal-dialog modal-dialog-scrollable modal-lg",
        div(
          class = "modal-content",
          div(
            class="modal-header",
            div(
              class="modal-title text-center h5",
              style=" color:#000000",
              id="sourcesModalLabel",
              "Data sources"
            )
          ),
          div(
            class = "py-3 px-5",
            style = "overflow-y:scroll; color:#000000",
            HTML("
        <b>Data sources used in this tool</b>"
            ),
            div(
              tags$li(
                class ="px-5 my-1","Data source 1 ",
                a(href = "http://google.com", "link", "target" = "_blank")
              ),
              tags$li(
                class ="px-5 my-1",
                "Data source 2",
                a(href = "http://google.com", "link", "target" = "_blank")
              )
            ),
            
            
            br(),
            HTML('
      <div class="modal-footer">
        <button type="button" class="btn btn-secondary" data-bs-dismiss="modal">Close</button>
      </div>')
          )
        ))
    ),
    
    # About modal
    div(
      class="modal fade", id="infoModal", tabindex="-1", "aria-labelledby"="infoModalLabel",  "aria-hidden"="true",
      div(
        class="modal-dialog modal-dialog-scrollable modal-lg",
        div(
          class = "modal-content px-5",
          div(
            class = "p-3 mt-3",
            id = "info-modal",
            style = "overflow-y:auto; color:#000000",
            
            
            h3("About this dashboard"),
            
            HTML("
        <p>
        <p>Description of the app
        </p>
        
        <ul>
        <li>Point 1</li>
        
        <li>Point 2</li>
        
        <li>Point 3</li>
        
        </ul>
        
        <br>
        ")
          ),
          
          
          HTML('
      <div class="modal-footer">
        <button type="button" class="btn btn-secondary" data-bs-dismiss="modal">Close</button>
      </div>')
        )
        
      )
    )
  )
  
}
