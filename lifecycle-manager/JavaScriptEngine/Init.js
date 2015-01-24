// voit lisää oman pelikijaston pelin sisällä tällä
//var s = document.createElement("script");
//s.type = "text/javascript";
//s.src = "http://somedomain.com/somescript";
//$("head").append(s);




var games = [
    "./../mini-games/collectTheBalls/index.html",
    "./../mini-games/dontHurtYourself/index.html",
    "./../mini-games/shootEmUp/index.html"
    ];
    
    
var life = 5;
var currentGame = -1;
var player1score = life;
var player2score = life;


function Init(){
    $("#player1score").text(life);
    $("#player2score").text(life);
    
}


jQuery.fn.rotate = function(degrees) {
    $(this).css({'-webkit-transform' : 'rotate('+ degrees +'deg)',
                 '-moz-transform' : 'rotate('+ degrees +'deg)',
                 '-ms-transform' : 'rotate('+ degrees +'deg)',
                 'transform' : 'rotate('+ degrees +'deg)'});
    return $(this);
};

function countScores(val){
        
    if(val === 1){
        $("#player1score").text(player2score--);
    }
    else
    if (val === 2){
        $("#player1score").text(player1score--);
    }
}

function startNextGame(e,parameter) {
    countScores(parameter);
    
    $("#gameFrame").attr("src", nextGameUrl());
    $("#gameFrame").focus();
    
}

function nextGameUrl() {
    currentGame++;
    if(currentGame === games.length)
        return games[0];
    return games[currentGame];
}

$(document).ready(function(){
    Init();
    
    $(document).bind('onGameEnd',startNextGame);
    // center the game hub to the screen
    
        $('#startGameButton').click(function () {
            startNextGame(null,0);
            var target = $('#gameFrame');
            if (target.length) {
                $('html,body').animate({
                    scrollTop: target.offset().top
                }, 1000);
                $("#gameFrame").focus();
                return false;
            }
        }
        );
    
});


