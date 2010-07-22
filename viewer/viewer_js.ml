

(*
I believe I have found an elegant solution to this:

JavaScript

/* important! for alignment, you should make things
 * relative to the canvas' current width/height.
 */
function draw() {
  var ctx = (a canvas context);
  ctx.canvas.width  = window.innerWidth;
  ctx.canvas.height = window.innerHeight;
  //...drawing code...
}

CSS

html, body {
  width:  100%;
  height: 100%;
  margin: 0px;
}

Hasn't had any large negative performance impact for me, so far.
*)

(*
<script>
window.onload = window.onresize = function() {
    var C = 0.8;        // canvas width to viewport width ratio
    var W_TO_H = 2/1;   // canvas width to canvas height ratio
    var el = document.getElementById("a");

    // For IE compatibility http://www.google.com/search?q=get+viewport+size+js
    var viewportWidth = window.innerWidth;
    var viewportHeight = window.innerHeight;

    var canvasWidth = viewportWidth * C;
    var canvasHeight = canvasWidth / W_TO_H;
    el.style.position = "fixed";
    el.setAttribute("width", canvasWidth);
    el.setAttribute("height", canvasHeight);
    el.style.top = (viewportHeight - canvasHeight) / 2;
    el.style.left = (viewportWidth - canvasWidth) / 2;

    window.ctx = el.getContext("2d");
    ctx.clearRect(0,0,canvasWidth,canvasHeight);
    ctx.fillStyle = 'yellow';
    ctx.moveTo(0, canvasHeight/2);
    ctx.lineTo(canvasWidth/2, 0);
    ctx.lineTo(canvasWidth, canvasHeight/2);
    ctx.lineTo(canvasWidth/2, canvasHeight);
    ctx.lineTo(0, canvasHeight/2);
    ctx.fill()
}
</script>

<body>
<canvas id="a" style="background: black">
</canvas>
</body>
*)
