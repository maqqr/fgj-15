
var lvlWidth = 800;
var lvlHeight = 600;
var coolDownForShooting = 0.5;

function Player(sprite, game, speed, x, y, playerNumber){
	this.speed = speed;
	this.score = 0;
	this.playerNumber = playerNumber;
	this.isActive = false;
	this.isLoading = false;
	this.lastShot = 0;
	this.loaded = 0;
	this.sprite = game.add.sprite(x, y, sprite)
	game.physics.arcade.enable(this.sprite);
	this.sprite.body.collideWorldBounds =true; 
}



Player.prototype.move = function(x, y){
	this.sprite.body.x += x;
	this.sprite.body.y += y;
}



function PlayerWithKeys(left, right, up, down, primary, special, sprite, game, speed){
	var player = new Player(sprite, game, speed);
	player.left = left;
	player.right = right;
	player.up = up;
	player.down = down;
	player.primary = primary;
	player.special = special;
	this.speed = speed;
	this.sprite = game.add.sprite(100, 100, sprite)
	game.physics.arcade.enable(this.sprite);
	return player;
}

Player.prototype.setActivity = function(activity){
	this.isActive = activity;
	this.sprite.body.velocity.x = 0;
	this.sprite.body.velocity.y = 0;
}

Player.prototype.registerPlayerAs = function(playerNumber){
	if(playerNumber == 1){
	this.left = "J".charCodeAt(0);
	this.right = "L".charCodeAt(0);
	this.up = "I".charCodeAt(0);
	this.down = "K".charCodeAt(0);
	this.primary = "N".charCodeAt(0);
	this.special = "M".charCodeAt(0);
	}
	else{
	this.left = "S".charCodeAt(0);
	this.right = "F".charCodeAt(0);
	this.up = "E".charCodeAt(0);
	this.down = "D".charCodeAt(0);
	this.primary = "A".charCodeAt(0);
	this.special = "Q".charCodeAt(0);
	
	}
	
}


Player.prototype.update = function(game, balls){
	if(!this.isActive) return;
	
	else if(game.input.keyboard.isDown(this.primary))
	{
		this.sprite.body.velocity.x = 0;
		this.sprite.body.velocity.y = 0;
		this.isLoading = true;
		this.loaded++;
		return;
	}
	else if (this.isLoading){
		this.shootMiddle(game, balls);
	}
	else this.isLoading = false;
	
	if(game.input.keyboard.isDown(this.down))
	{
		move(0, this.speed, this.sprite);
	}
	else if(game.input.keyboard.isDown(this.up))
	{
		move(0, -this.speed, this.sprite);
	}
	else
	{
		this.sprite.body.velocity.x = 0;
		this.sprite.body.velocity.y = 0;
	}

}

Player.prototype.shootMiddle = function(game, balls){
	if(this.loaded < 5 || game.time.totalElapsedSeconds() <= this.lastShot + coolDownForShooting ){
		this.loaded = 0;
		return;
	}
	this.isLoading = false;
	var xPos = (this.playerNumber == 1 ? 50 : -50); 
	var projectile = game.add.sprite(this.sprite.body.x + xPos, this.sprite.body.y, this.playerNumber == 1 ? 'ball' : 'ballBlue');
	game.physics.arcade.enable(projectile);
	var velocityX = 100.0;
	velocityX *= 0.1 * this.loaded;
	if(this.playerNumber == 1)
	{
		
	}
	else{
		velocityX *= -1.0;
	}
	var velocityY = 0.0;
	if(game.input.keyboard.isDown(this.up))
		velocityY = -this.loaded;
	else if(game.input.keyboard.isDown(this.down))
		velocityY = this.loaded ;
	projectile.body.velocity.x = velocityX;
	balls[balls.length] = projectile;
	this.loaded = 0;
	this.lastShot = game.time.totalElapsedSeconds();
	if(velocityY != 0){

		game.time.events.add(Phaser.Timer.SECOND * 0.1, function(){
			projectile.body.velocity.y += velocityY;
		}, this);
		game.time.events.add(Phaser.Timer.SECOND * 0.2, function(){
			projectile.body.velocity.y += velocityY;
		}, this);
		game.time.events.add(Phaser.Timer.SECOND * 0.3, function(){
			projectile.body.velocity.y += velocityY*2;
		}, this);
		game.time.events.add(Phaser.Timer.SECOND * 0.5, function(){
			projectile.body.velocity.x *= 0.75;
			projectile.body.velocity.y += velocityY*2;
		}, this);
	}
}


function moveWithinField(xToMove, yToMove, characterObject){
	if(characterObject.body.x < 0 + characterObject.body.halfWidth)
		characterObject.body.x += lvlWidth;
	else if(characterObject.body.y < 0 - characterObject.body.halfHeight)
		characterObject.body.y += lvlHeight;
	else if(characterObject.body.y > lvlHeight - characterObject.body.halfHeight)
		characterObject.body.y += - lvlHeight;
	else if(characterObject.body.x > lvlWidth - characterObject.body.halfWidth)
		characterObject.body.x += -lvlWidth;
	move(xToMove, yToMove, characterObject);
}

function move(xToMove, yToMove, characterObject){
	characterObject.body.velocity.x = xToMove;
	characterObject.body.velocity.y = yToMove;
}

