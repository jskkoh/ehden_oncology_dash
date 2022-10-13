document.addEventListener('DOMContentLoaded', (event) => {

// fade out intro page
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


  

    // speedy way to remove intro page: just press enter
    window.addEventListener('keydown', function(e){
        if (e.keyCode == 13) {
            document.querySelector("#close_intro").click()
        }
      });


      // link to ehden website
      document.querySelector("#ehden").addEventListener("click", () => {
        window.open('https://www.ehden.eu/', '_blank')
      });


  // author tooltip
    var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle="tooltip"]'))
    var tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) {
      return new bootstrap.Tooltip(tooltipTriggerEl)
    })

      var exampleEl = document.querySelector("#credits")
      var original = exampleEl.getAttribute('data-bs-original-title');
      var tooltip = new bootstrap.Tooltip(exampleEl)
      
    document.querySelector("#credits").addEventListener("click", () => {
     /* Copy the text inside the text field */
      navigator.clipboard.writeText(original);
      document.querySelector("#credits").setAttribute('data-bs-original-title', "Copied to cliboard!");
      tooltip.show()
      setTimeout(function() {
        tooltip.hide()
        document.querySelector("#credits").setAttribute('data-bs-original-title', original);
      }, 2000); 
      
      
    });


  })




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