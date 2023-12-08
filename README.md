<a><img src="https://github.com/ofurkancoban/ForecastingMethods_DataAnalysis_Projects/blob/main/img/bg.png" alt="Kaggle"></a>

# University of Oldenburg Forecasting Methods Course Projects

## Introduction

Hi there! I am a student at the University of Oldenburg, currently enrolled in the Forecasting Methods course. In this repository, I'm thrilled to share my individual data analysis projects that I've developed as part of this course. Here, you'll find how I've applied various forecasting methodologies to predict future trends and patterns.

### About the Course

The Forecasting Methods course has been an incredible journey, introducing me to both the fundamental and advanced techniques in forecasting. I've explored topics like time series analysis and statistical methods, applying them in practical, real-world contexts.

### My Projects

Each folder in this repository represents an individual project that I've worked on. These projects reflect my application of forecasting methods on diverse datasets, aiming to provide insights into various industries and sectors.

- **Project 1:** _<a href="https://github.com/ofurkancoban/ForecastingMethods_DataAnalysis_Projects/tree/main/ProblemSet1_STAN_db" target="_self">STAN Database</a>_  - The STAN database is a comprehensive statistical dataset that integrates sectoral and national accounts, providing detailed insights into economic activities across various industries in different countries.


### Tools and Technologies

Throughout these projects, I have used a variety of tools and technologies, including:

- R 

### Collaboration and Feedback

While these projects are my individual efforts, I am open to collaborations and would greatly appreciate any feedback or suggestions. Feel free to open an issue or submit a pull request if you have any ideas or inputs.

### Contact Me

For more information about any of these projects or the course, please don't hesitate to reach out to me:


### Furkan Coban
- <a href="mailto:ofurkancoban@gmail.com" target="_blank"><img src="https://github.com/ofurkancoban/ForecastingMethods_DataAnalysis_Projects/blob/main/img/email.png" alt="Kaggle"></a>
- <a href="https://www.linkedin.com/in/ofurkancoban" target="_blank"><img src="https://github.com/ofurkancoban/ForecastingMethods_DataAnalysis_Projects/blob/main/img/in.png" alt="Kaggle"></a>
- <a href="https://www.kaggle.com/ofurkancoban" target="_blank"><img src="https://github.com/ofurkancoban/ForecastingMethods_DataAnalysis_Projects/blob/main/img/kaggle.png" alt="Kaggle"></a>



<style type="text/css">
#outerCircleText {
font-style: italic;
font-weight: bold;
font-family: 'comic sans ms', verdana, arial;
color: #000;

position: absolute;top: 0;left: 0;z-index: 3000;cursor: default;}
#outerCircleText div {position: relative;}
#outerCircleText div div {position: absolute;top: 0;left: 0;text-align: center;}
</style>

<script type="text/javascript">

;(function(){

var msg = "webkodu.ozgurlukicin.com 'a Hoşgeldiniz !";


var size = 24;

var circleY = 0.75; var circleX = 2;

var letter_spacing = 5;

var diameter = 10;

var rotation = 0.4;

var speed = 0.3;


if (!window.addEventListener && !window.attachEvent || !document.createElement) return;

msg = msg.split('');
var n = msg.length - 1, a = Math.round(size * diameter * 0.208333), currStep = 20,
ymouse = a * circleY + 20, xmouse = a * circleX + 20, y = [], x = [], Y = [], X = [],
o = document.createElement('div'), oi = document.createElement('div'),
b = document.compatMode && document.compatMode != "BackCompat"? document.documentElement : document.body,

mouse = function(e){
 e = e || window.event;
 ymouse = !isNaN(e.pageY)? e.pageY : e.clientY;
 xmouse = !isNaN(e.pageX)? e.pageX : e.clientX; 
},

makecircle = function(){
 if(init.nopy){
  o.style.top = (b || document.body).scrollTop + 'px';
  o.style.left = (b || document.body).scrollLeft + 'px';
 };
 currStep -= rotation;
 for (var d, i = n; i > -1; --i){ 
  d = document.getElementById('iemsg' + i).style;
  d.top = Math.round(y[i] + a * Math.sin((currStep + i) / letter_spacing) * circleY - 15) + 'px';
  d.left = Math.round(x[i] + a * Math.cos((currStep + i) / letter_spacing) * circleX) + 'px';
 };
},

drag = function(){
 y[0] = Y[0] += (ymouse - Y[0]) * speed;
 x[0] = X[0] += (xmouse - 20 - X[0]) * speed;
 for (var i = n; i > 0; --i){
  y[i] = Y[i] += (y[i-1] - Y[i]) * speed;
  x[i] = X[i] += (x[i-1] - X[i]) * speed;
 };
 makecircle();
},

init = function(){ 
 if(!isNaN(window.pageYOffset)){
  ymouse += window.pageYOffset;
  xmouse += window.pageXOffset;
 } else init.nopy = true;
 for (var d, i = n; i > -1; --i){
  d = document.createElement('div'); d.id = 'iemsg' + i;
  d.style.height = d.style.width = a + 'px';
  d.appendChild(document.createTextNode(msg[i]));
  oi.appendChild(d); y[i] = x[i] = Y[i] = X[i] = 0;
 };
 o.appendChild(oi); document.body.appendChild(o);
 setInterval(drag, 25);
},

ascroll = function(){
 ymouse += window.pageYOffset;
 xmouse += window.pageXOffset;
 window.removeEventListener('scroll', ascroll, false);
};

o.id = 'outerCircleText'; o.style.fontSize = size + 'px';

if (window.addEventListener){
 window.addEventListener('load', init, false);
 document.addEventListener('mouseover', mouse, false);
 document.addEventListener('mousemove', mouse, false);
  if (/Apple/.test(navigator.vendor))
   window.addEventListener('scroll', ascroll, false);
}
else if (window.attachEvent){
 window.attachEvent('onload', init);
 document.attachEvent('onmousemove', mouse);
};

})();

</script>
