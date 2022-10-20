document.addEventListener('DOMContentLoaded', (event) => {

// Intro page
    function removeFadeOut( el, speed , redraw = false) {
        var seconds = speed/1000;
        el.style.transition = "opacity "+seconds+"s ease";

        el.style.opacity = 0;
        setTimeout(function() {
            el.parentNode.removeChild(el);
        }, speed);
    }

    dom = document.querySelector("body");
    overlay = dom.getElementsByClassName("waiter-overlay");

    current_progress = 0,
    step = 0.75;
    interval = setInterval(function() {
        current_progress += step;
        progress = Math.round(Math.atan(current_progress) / (Math.PI / 2) * 102 ) 
        $(".progress-bar")
            .css("width", progress + "%")
            .attr("aria-valuenow", progress)
            .text(progress + "%");
            
        if (progress >= 100){
            removeFadeOut(overlay[0], 1000, true)
            clearInterval(interval)
        }
        
    }, 100);


  

    // Remove intro page (just press enter)
    window.addEventListener('keydown', function(e){
        if (e.keyCode == 13) {
            document.querySelector("#close_intro").click()
        }
      });


      // Link to ehden website
      document.querySelector("#ehden").addEventListener("click", () => {
        window.open('https://www.ehden.eu/', '_blank')
      });




  })



// Highcart renderer
Highcharts.SVGRenderer.prototype.symbols.download = function (x, y, w, h) {
    var path = [
        // Arrow stem
        'M', x + w * 0.5, y,
        'L', x + w * 0.5, y + h * 0.7,
        // Arrow head
        'M', x + w * 0.3, y + h * 0.5,
        'L', x + w * 0.5, y + h * 0.7,
        'L', x + w * 0.7, y + h * 0.5,
        // Box
        'M', x, y + h * 0.9,
        'L', x, y + h,
        'L', x + w, y + h,
        'L', x + w, y + h * 0.9
    ];
    return path;
};