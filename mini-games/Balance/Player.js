
var lvlWidth = 800;
var lvlHeight = 600;

function Player(sprite, game, speed, x, y, playerNumber){
	this.speed = speed;
	this.score = 0;
	this.playerNumber = playerNumber;
	this.isActive = false;
	this.canMoveDiagonal = false;
	this.sprite = game.add.sprite(x, y, sprite)
	game.physics.arcade.enable(this.sprite);
	this.fallingAngle = this.sprite.angle;
	this.sprite.anchor.setTo(0.5, 1);

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
	this.fallingAngle = 0;
	this.sprite = game.add.sprite(100, 100, sprite)
	game.physics.arcade.enable(this.sprite);
	return player;
}

Player.prototype.setActivity = function(activity){
	this.isActive = activity;
	this.sprite.body.velocity.x = 0;
	this.sprite.body.velocity.y = 0;
	this.sprite.body.angularVelocity = 0;
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


Player.prototype.update = function(game, angleChange, overTimeChange){
	if(!this.isActive) return;
	
	//this.sprite.body.angularVelocity += overTimeChange;
	if(this.sprite.body.angularVelocity > 180)
		this.sprite.body.angularVelocity = 180;
	if(this.sprite.body.angularVelocity < -180)
		this.sprite.body.angularVelocity = -180;
	
	if(game.input.keyboard.isDown(this.left))
	{
		this.sprite.body.angularVelocity -= angleChange;
//		moveWithinField(-this.speed, 0, this.sprite);
	}
	else if(game.input.keyboard.isDown(this.right))
	{
		this.sprite.body.angularVelocity += angleChange;
//		moveWithinField(this.speed, 0, this.sprite);
	}
	this.sprite.body.angularVelocity += this.sprite.body.angularVelocity > 0 ? overTimeChange : -overTimeChange;
	
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

