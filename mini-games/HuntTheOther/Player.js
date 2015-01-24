
var lvlWidth = 800;
var lvlHeight = 600;
var coolDownForShooting = 0.4;

function Player(sprite, game, speed, x, y, playerNumber){
	this.speed = speed;
	this.score = 0;
	this.playerNumber = playerNumber;
	this.isActive = false;
	this.shoot = function(){};
	this.lastShot = 0;
	this.canMoveDiagonal = false;
	this.sprite = game.add.sprite(x, y, sprite)
	game.physics.arcade.enable(this.sprite);


}


Player.prototype.setBounce = function(x, y){
	this.sprite.body.bounce.setTo(x, y);
}



Player.prototype.destroy = function(){
	this.sprite.destroy();
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


Player.prototype.update = function(game){
	if(!this.isActive) return;
	
		if(game.input.keyboard.isDown(this.primary))
		{
			if(game.time.totalElapsedSeconds() > this.lastShot + coolDownForShooting ){
				this.shoot();
				this.lastShot = game.time.totalElapsedSeconds();
		}

	}
	if(this.canMoveDiagonal){
		var xSpeed = 0;
		var ySpeed = 0;
		if(game.input.keyboard.isDown(this.left))
		{
			xSpeed = -this.speed;
		}
		else if(game.input.keyboard.isDown(this.right))
		{
			xSpeed = this.speed;
		}
		if(game.input.keyboard.isDown(this.down))
		{
			ySpeed= this.speed;
		}
		else if(game.input.keyboard.isDown(this.up))
		{
			ySpeed= -this.speed;
		}
		moveWithinField(xSpeed, ySpeed, this.sprite);
	}else{
		if(game.input.keyboard.isDown(this.left))
		{
			moveWithinField(-this.speed, 0, this.sprite);
		}
		else if(game.input.keyboard.isDown(this.right))
		{
			moveWithinField(this.speed, 0, this.sprite);
		}
		else if(game.input.keyboard.isDown(this.down))
		{
			moveWithinField(0, this.speed, this.sprite);
		}
		else if(game.input.keyboard.isDown(this.up))
		{
			moveWithinField(0, -this.speed, this.sprite);
		}
		else
		{
			this.sprite.body.velocity.x = 0;
			this.sprite.body.velocity.y = 0;
		}
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

