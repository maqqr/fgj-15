//   _____          __  __ ______   _      _  __                     _        __  __                                      ___           __  
//  / ____|   /\   |  \/  |  ____| | |    (_)/ _|                   | |      |  \/  |                                    / / |          \ \ 
// | |  __   /  \  | \  / | |__    | |     _| |_ ___  ___ _   _  ___| | ___  | \  / | __ _ _ __   __ _  __ _  ___ _ __  | || |_ _ __ ___ | |
// | | |_ | / /\ \ | |\/| |  __|   | |    | |  _/ _ \/ __| | | |/ __| |/ _ \ | |\/| |/ _` | '_ \ / _` |/ _` |/ _ \ '__| | || __| '_ ` _ \| |
// | |__| |/ ____ \| |  | | |____  | |____| | ||  __/ (__| |_| | (__| |  __/ | |  | | (_| | | | | (_| | (_| |  __/ |    | || |_| | | | | | |
//  \_____/_/    \_\_|  |_|______| |______|_|_| \___|\___|\__, |\___|_|\___| |_|  |_|\__,_|_| |_|\__,_|\__, |\___|_|    | | \__|_| |_| |_| |
//                                                         __/ |                                        __/ |            \_\            /_/ 
//                                                        |___/                                        |___/                                
//
//
//  qa___        ___             _     
// | _ )_  _  / __| __ _ _ __ (_)_ _ 
// | _ \ || | \__ \/ _` | '  \| | '_|
// |___/\_, | |___/\__,_|_|_|_|_|_|  
//      |__/                         

var defaultGameNumber = -1;

var games = [
    "./../mini-games/FLAPPY/index.html",
    "./../mini-games/HuntTheOther/index.html",
    "./../mini-games/PRESSMOREBUTTONS/index.html",
    "./../mini-games/islandFight/index.html",
    "./../mini-games/nappainsarja/index.html",
    "./../mini-games/avoidTheFIRAAH/index.html",
    "./../mini-games/starCollection/index.html",
    "./../mini-games/shootEmUp/index.html",
    "./../mini-games/collectTheBalls/index.html",
    "./../mini-games/dontHurtYourself/index.html"
    ];
    
var life = 5;
var currentGame = -1;
var player1score = life;
var player2score = life;
var previousGame = -1;

jQuery.fn.rotate = function(degrees) {
    $(this).css({'-webkit-transform' : 'rotate('+ degrees +'deg)',
                 '-moz-transform' : 'rotate('+ degrees +'deg)',
                 '-ms-transform' : 'rotate('+ degrees +'deg)',
                 'transform' : 'rotate('+ degrees +'deg)'});
    return $(this);
};

$(document).ready(function(){
    
    $("#main_canvas").hide();
    $(".player").hide();
    // center the game hub to the screen

    $('body,iframe').keypress(function (e) {
        if (e.which == '98') { // start on 'b'
            Init();
        }
        if (e.which == '121') { // seuraava peli on 'y'
            Init();
        }
    });
   
    
    $('#startGameButton').click(function () {
        Init();
    }
    );    
});

function Init(){
    $("#player1score").text(life);
    $("#player2score").text(life);
    $("#main_canvas").show( 500 ,"swing", showGameHubAndStartGame);
    playMainSong();
    $(document).bind('onGameEnd',startNextGame);
    $(document).bind('onTimeUpdate',updateTime);
}


function updateTime(e, parameter) {
    $("#timer").text(parameter);
}


function showGameHubAndStartGame(){
    startNextGame(null, 0);
    var target = $('#gameFrame');
    if (target.length) {
        $('html,body').animate({
            scrollTop: target.offset().top
        }, 1000);
        $("#gameFrame").focus();
        return false;
    }
}


function startNextGame(e, parameter) {
    countScores(parameter);
    if (e){
        $("#gameFrame").hide(500, function () {
            $("#gameFrame").attr("src", nextGameUrl());
        });
        $("#gameFrame").show(500);
    }
    else
        $("#gameFrame").attr("src", nextGameUrl());
    
    $("#gameFrame").focus();
}

function nextGameUrl() {
    previousGame = currentGame;
    while (true) {
        currentGame = randomInt(0, games.length - 1);
        if (currentGame !== previousGame)
            break;
    }
    if(defaultGameNumber !== -1) // estää randomisoinnin
        return games[defaultGameNumber];;
    return games[currentGame];
}

function randomInt(min,max) {
    return Math.floor((Math.random() * max) + min);
}

function countScores(val){
    if(val === 1){
        $("#player2score").text(--player2score);
    }
    else
    if (val === 2){
        $("#player1score").text(--player1score);
    }

    checkGameEnd();
}

function checkGameEnd() {
    if(player2score < 0) {
        alert("player1 won !!");
        endGame();
    }
    if(player1score < 0) {
        alert("player2 won !!");
        endGame();
    }
    
}

function endGame() {
    location.reload();
    $("#main_canvas").hide(500);
}