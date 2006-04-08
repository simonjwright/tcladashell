/* chaser.js
 * by Aaron Boodman v1.0 000919
 * Copyright (c) 2000 Aaron Boodman. All Rights Reserved.
 * Created for GreatEqualizer.com (http://www.greatequalizer.com/) and
 * documented at DHTML Lab (http://www.webreference.com/dhtml/)
 * License to use is granted if and only if this entire
 * copyright notice is included.
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
*/

// The chaser object. Since we don't anticipate having more
// than one on a page, we don't bother making this into
// a class definition. All necessary properties are set as
// properties of this object to avoid global variables.

var oChaser = {
	topMargin	: 85,
	callRate	: 10,
	slideTime	: 1200,
	maxDiff		: document.all ? document.body.clientHeight : window.innerHeight,
	isIE		: document.all ? true : false,
	chaserDiv	: document[document.all ? "all" : "layers"]["myChaser"]
}

// Arrange to have the main loop called as often as possible, 
// but not more often than every 35 milliseconds.
// Even though some OS's can acheive better than that, there's no real reason to
// overload them if its not going to improve the animation quality very much.
// I tested, and better than 35ms doesn't do very much visually.
window.setInterval("oChaser.main( )", oChaser.callRate)


// Main loop. Updates targetY, and decides whether to start
// the animation over again, continue an existing animation,
// or do nothing at all.
oChaser.main = function( )
{
	this.currentY	= this.isIE ? this.chaserDiv.style.pixelTop : this.chaserDiv.top
	this.scrollTop	= this.isIE ? document.body.scrollTop : window.pageYOffset
	var newTargetY	= this.scrollTop + this.topMargin
	
	if ( this.currentY != newTargetY ) {

		if ( newTargetY != this.targetY ) {

			this.targetY = newTargetY
			this.slideInit( )
	
		}

		this.slide( )
		
	}
}



// .slideInit( ). Initializes the slide animation. Sets properties
// of the oChaser object that will represent the various paramaters
// for the sine wave function.
oChaser.slideInit = function( )
{
	var now	= new Date( )
	
	this.A		= this.targetY - this.currentY
	this.B		= Math.PI / ( 2 * this.slideTime )
	this.C		= now.getTime( )

	if (Math.abs(this.A) > this.maxDiff) {
		this.D = this.A > 0 ? this.targetY - this.maxDiff : this.targetY + this.maxDiff
		this.A = this.A > 0 ? this.maxDiff : -this.maxDiff
	} else {
		this.D = this.currentY
	}
}



// .slide( ). Moves the oChaser one frame. Its rate decreases and
// is defined by a sine wave.
oChaser.slide = function( )
{
	var now	= new Date( )
	var newY	= this.A * Math.sin( this.B * ( now.getTime( ) - this.C ) ) + this.D
	newY		= Math.round( newY )

	if (( this.A > 0 && newY > this.currentY ) ||
		( this.A < 0 && newY < this.currentY )) {
			
			if ( this.isIE )this.chaserDiv.style.pixelTop = newY
			else			this.chaserDiv.top = newY
	}
}