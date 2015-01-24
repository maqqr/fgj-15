// voit lisää oman pelikijaston pelin sisällä tällä
//var s = document.createElement("script");
//s.type = "text/javascript";
//s.src = "http://somedomain.com/somescript";
//$("head").append(s);

$(document).ready(function(){
   // do jQuery

    $("#startGame").click(function () { 
       $("#gameFrame").attr("src", "./../mini-games/dontHurtYourself/index.html");
    });
});