# Intro page with loading progress bar

landingDiv = function(){
  
  
  HTML('
    <div class="landing mx-auto px-sm-2 px-3 py-1 mb-3 d-flex flex-column ">
         <div class="intro-text intro-title display-1 mx-auto">
                      EHDEN cancer oncology survival tool 
                    </div>
                    <div class="intro-text fs-2 intro-subtitle mx-auto my-5">
                        Danielle Newby, James Love-Koh, Ravinder Claire etc...
                    </div>
                    <div class = "logos d-flex flex-row justify-content-center align-items-center flex-wrap mx-auto px-5">
                        <div class="cell">
                            <img  class ="image" src="ehden_logo.png">
                        </div>
                        <div class="cell">
                            <img class ="image" src="nice_logo.png">
                        </div>
                    </div>
                    <div class="progress">
                      <div class="progress-bar" role="progressbar" style="width: 0%" aria-valuenow="0" aria-valuemin="0" aria-valuemax="100"></div>
                    </div>
         </div>')
  
}