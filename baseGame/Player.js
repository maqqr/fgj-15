var lvlWidth = 800;
var lvlHeight = 600;

function Player(sprite, game, speed){
	this.speed = speed;
	this.score = 0;
	this.isActive = false;
	this.sprite = game.add.sprite(100, 100, sprite)
	game.physics.arcade.enable(this.sprite);
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
	if(game.input.keyboard.isDown(this.left))
	{
		move(-this.speed, 0, this.sprite);
	}
	else if(game.input.keyboard.isDown(this.right))
	{
		move(this.speed, 0, this.sprite);
	}
	else if(game.input.keyboard.isDown(this.down))
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

