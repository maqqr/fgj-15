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
//  ___        ___             _     
// | _ )_  _  / __| __ _ _ __ (_)_ _ 
// | _ \ || | \__ \/ _` | '  \| | '_|
// |___/\_, | |___/\__,_|_|_|_|_|_|  
//      |__/                         

var defaultGameNumber = 2; // -1 niin randomi päällä

var games = [
    "./../mini-games/purpleBall/index.html",
    "./../mini-games/Balance/index.html",
    "./../mini-games/crushTheHonda/index.html",
    "./../mini-games/mathMadness/index.html",
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
    
var life = 10;
var linearGameChangeEnabled = true ;
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
     $(".player1win,.player2win,.tie1,.tie2").hide();
     if(defaultGameNumber === -1)
        games = shuffle(games);
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

function shuffle(o){ //v1.0
    for(var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
};

function nextGameUrl() {
    if(linearGameChangeEnabled) {
        linearNextGame();
    }
    else {
        randomize();
    }
    if(defaultGameNumber !== -1) // estää randomisoinnin
        return games[defaultGameNumber];;
    return games[currentGame];
}

function linearNextGame() {
    ++currentGame ;
    if(currentGame  === games.length) {
        linearGameChangeEnabled = false;
        randomize();
    }
    
}

function randomize() {
    previousGame = currentGame;
    while (true) {
        currentGame = randomInt(0, games.length );
        if (currentGame !== previousGame)
            break;
    }
    
}

function randomInt(min,max) {
    return Math.floor((Math.random() * max) + min);
}

function countScores(val){
    $(".player1win, .player2win, .tie1,.tie2").hide();
    if(val === 1){
        $("#player2score").text(--player2score);
        $(".player1win").show();
    }
    else
    if (val === 2){
        $("#player1score").text(--player1score);
        $(".player2win").show();
    }
else {
    $(".tie1,.tie2").show();
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