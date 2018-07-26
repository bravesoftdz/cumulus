package com.roytanck.wpcumulus {
	public class TagCloud extends MovieClip	{

		public function TagCloud(){
		 // load or parse the data
			myXML = new XML();
			if( this.loaderInfo.parameters.mode == null )	{
				// base url
				var a:Array = this.loaderInfo.url.split("/");
				a.pop();
				var baseURL:String = a.join("/") + "/";
				// load XML file
				var XMLPath = ( this.loaderInfo.parameters.xmlpath == null ) ? baseURL + "tagcloud.xml" : this.loaderInfo.parameters.xmlpath;
				var myXMLReq:URLRequest = new URLRequest( XMLPath );
				var myLoader:URLLoader = new URLLoader(myXMLReq);
				myLoader.addEventListener("complete", xmlLoaded);
				function xmlLoaded(event:Event):void {
						myXML = XML(myLoader.data); // test with tags from XML file
						//myXML = new XML("<tags></tags>"); // test without tags
						//addCategories( categories ); // add categories
						init( myXML );
				}
			} else {
				switch( this.loaderInfo.parameters.mode ){
					case "tags":
						myXML = new XML( this.loaderInfo.parameters.tagcloud );
						break;
					case "cats":
						myXML = new XML("<tags></tags>");
						addCategories( this.loaderInfo.parameters.categories );
						break;
					default:
						myXML = new XML( this.loaderInfo.parameters.tagcloud );
						addCategories( this.loaderInfo.parameters.categories );
						break;
				}
				init( myXML );
			}
		}

		private function addCategories( cats:String ){
			// unescape leave spaces as '+', so we have to filter these out manually
			cats = unescape(cats).replace(/\+/g, " ");
			// use the fact that WP outputs line breaks to split the string into bits
			var cArray:Array = cats.split("<br />");
			// loop though them to find the smallest and largest 'tags'
			var smallest:Number = 9999;
			var largest:Number = 0;
			var pattern:RegExp = /\d/g;
			for( var i:Number=0; i<cArray.length-1; i++ ){
				var parts:Array = cArray[i].split( "</a>" );
				// user regular extpressions to get rid of extra stuff
				var nr:Number = Number( parts[1].match(pattern).join("") );
				largest = Math.max( largest, nr );
				smallest = Math.min( smallest, nr );
			}
			// how much must we scale the categories to match the tags?
			var scalefactor:Number = ( smallest == largest )? 7/largest : 14 / largest;
			// loop through them again and add to XML
			for( i=0; i<cArray.length-1; i++ ){
				parts = cArray[i].split( "</a>" );
				nr = Number( parts[1].match(pattern).join("") );
				var node:String = "<a style=\"" + ((nr*scalefactor)+8) + "\"" + parts[0].substr( parts[0].indexOf("<a")+2 ) + "</a>";
				myXML.appendChild( node );
			}
		}

		private function init( o:XML ):void {
			// create movie clips
			for each( var node2:XML in o.a ){
				// figure out what color it should be
				var nr2:Number = getNumberFromString( node2["@style"] );
				var perc:Number = ( smallest == largest ) ? 1 : (nr2-smallest) / (largest-smallest);
				// create mc
				var col:Number = ( node2["@color"] == undefined ) ? getColorFromGradient( perc ) : Number( node2["@color"] );
				var hicol:Number = ( node2["@hicolor"] == undefined ) ? ( ( hicolor == tcolor ) ? getColorFromGradient( perc ) : hicolor ) : Number( node2["@hicolor"] );
				var mc:Tag = new Tag( node2, col, hicol );
				holder.addChild(mc);
				// store reference
				mcList.push( mc );
			}
		}

		private function updateTags( e:Event ):void {
			var a:Number;
			var b:Number;
			if( active ){
				a = (-Math.min( Math.max( holder.mouseY, -250 ), 250 ) / 150 ) * tspeed;
				b = (Math.min( Math.max( holder.mouseX, -250 ), 250 ) /150 ) * tspeed;
			} else {
				a = lasta * 0.98;
				b = lastb * 0.98;
			}
			lasta = a;
			lastb = b;
			// if a and b under threshold, skip motion calculations to free up the processor
			if( Math.abs(a) > 0.01 || Math.abs(b) > 0.01 ){
				var c:Number = 0;
				sineCosine( a, b, c );
				// bewegen van de punten
				for( var j:Number=0; j<mcList.length; j++ ) {
					// multiply positions by a x-rotation matrix
					var rx1:Number = mcList[j].cx;
					var ry1:Number = mcList[j].cy * ca + mcList[j].cz * -sa;
					var rz1:Number = mcList[j].cy * sa + mcList[j].cz * ca;
					// multiply new positions by a y-rotation matrix
					var rx2:Number = rx1 * cb + rz1 * sb;
					var ry2:Number = ry1;
					var rz2:Number = rx1 * -sb + rz1 * cb;
					// multiply new positions by a z-rotation matrix
					var rx3:Number = rx2 * cc + ry2 * -sc;
					var ry3:Number = rx2 * sc + ry2 * cc;
					var rz3:Number = rz2;
					// set arrays to new positions
					mcList[j].cx = rx3;
					mcList[j].cy = ry3;
					mcList[j].cz = rz3;
					// add perspective
					var per:Number = d / (d+rz3);
					// setmc position, scale, alpha
					mcList[j].x = rx3 * per;
					mcList[j].y = ry3 * per;
					mcList[j].scaleX = mcList[j].scaleY =  per;
					mcList[j].alpha = per/2;
				}
				depthSort();
			}
		}

		private function depthSort():void {
			mcList.sortOn( "cz", Array.DESCENDING | Array.NUMERIC );
			var current:Number = 0;
			for( var i:Number=0; i<mcList.length; i++ ){
				holder.setChildIndex( mcList[i], i );
				if( mcList[i].active == true ){
					current = i;
				}
			}
			holder.setChildIndex( mcList[current], mcList.length-1 );
		}

		/* See http://blog.massivecube.com/?p=9 */
		private function positionAll():void {
			var phi:Number = 0;
			var theta:Number = 0;
			var max:Number = mcList.length;
			// mix up the list so not all a' live on the north pole
			mcList.sort( function(){ return Math.random()<0.5 ? 1 : -1; } );
			// distibute
			for( var i:Number=1; i<max+1; i++){
				if( distr ){
					phi = Math.acos(-1+(2*i-1)/max);
					theta = Math.sqrt(max*Math.PI)*phi;
				}else{
					phi = Math.random()*(Math.PI);
					theta = Math.random()*(2*Math.PI);
				}
				// Coordinate conversion
				mcList[i-1].cx = radius * Math.cos(theta)*Math.sin(phi);
				mcList[i-1].cy = radius * Math.sin(theta)*Math.sin(phi);
				mcList[i-1].cz = radius * Math.cos(phi);
			}
		}
	}
}
