# Intro page with loading progress bar

landingDiv = function(){
  
  
  HTML('
    <div class="landing mx-auto px-sm-2 px-3 py-1 mb-3">
         <div class="intro-text intro-title display-1 mx-auto",
           style="font-size: 50px; font-weight:bold;"
         >
                      EHDEN Cancer Oncology Survival Dashboard
                    <br><br>
                    <div class = "logos d-flex flex-row justify-content-center align-items-center flex-wrap mx-auto px-5">
                        <br>
                            <img class ="image" src="ehden_logo.png" width="400px">
                        
                            <br><br>
                            <img class ="image" src="nice_logo.png" width="400px">
                    </div>
                    <div class="progress">
                      <div class="progress-bar" role="progressbar" style="width: 0%" aria-valuenow="0" aria-valuemin="0" aria-valuemax="100"></div>
                    </div>
         </div>')
  
}