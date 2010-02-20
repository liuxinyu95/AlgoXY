// Color library

// RGB object
function AmazonRGB(red, green, blue)
{
    // These are integers between 0 and 255 inclusive.
    this.r = red;
    this.g = green;
    this.b = blue;
}

// Accessor methods for the RGB components.
AmazonRGB.prototype.getR = function() { return this.r; };
AmazonRGB.prototype.getG = function() { return this.g; };
AmazonRGB.prototype.getB = function() { return this.b; };

// Defines and returns the 6-digit hex representation
// of this RGB color.
AmazonRGB.prototype.getHex = function() {
    if (!this.hex)
    {
        var map = '0123456789ABCDEF';
        this.hex = '' +
            map.substr(Math.floor(this.r / 16), 1) + 
            map.substr((this.r % 16), 1) +
            map.substr(Math.floor(this.g / 16), 1) + 
            map.substr((this.g % 16), 1) +
            map.substr(Math.floor(this.b / 16), 1) + 
            map.substr((this.b % 16), 1);
    }
    return this.hex;
};

// Returns the equivalent HSV object.
AmazonRGB.prototype.toHSV = function() {
    // Normalize RGB values for color space transform.
    var r = this.r / 255;
    var g = this.g / 255;
    var b = this.b / 255;

    // Helper values for color space transform.
    var max  = (r > g) ? r : g;
    max  = (max > b) ? max : b;
    var min  = (r < g) ? r : g;
    min  = (min < b) ? min : b;

    var delta = max - min;

    // Color space transform.
    var v = max;
    var s = 0;

    if (max > 0) { s = delta / max; }

    var h = 0;

    if (delta > 0)
    {
        if ((max == r) && (max != g))
        {
            h += (g - b) / delta;
        }
        if ((max == g) && (max != b))
        {
            h += (2 + (b - r) / delta);
        }
        if ((max == b) && (max != r))
        {
            h += (4 + (r - g) / delta);
        }
        h = h * 60;
        if (h < 0) { h += 360; }
    }

    var result = new AmazonHSV(h, s, v);
    return result;
};


// HSV object
function AmazonHSV(hue, saturation, value)
{
    // These are real numbers between 0 and 1 inclusive.
    this.h = hue;
    this.s = saturation;
    this.v = value;
}

// Accessor methods for the HSV components.
AmazonHSV.prototype.getH = function() { return this.h; };
AmazonHSV.prototype.getS = function() { return this.s; };
AmazonHSV.prototype.getV = function() { return this.v; };

// Returns the equivalent RGB object.
AmazonHSV.prototype.toRGB = function() {
    var sat_r;
    var sat_g;
    var sat_b;

    // Color space transform
    if (this.h < 120)
    {
        sat_r = (120 - this.h) / 60;
        sat_g = this.h / 60;
        sat_b = 0;
    }
    else if (this.h < 240)
    {
        sat_r = 0;
        sat_g = (240 - this.h) / 60;
        sat_b = (this.h - 120) / 60;
    }
    else 
    {
        sat_r = (this.h - 240) / 60;
        sat_g = 0;
        sat_b = (360 - this.h) / 60;
    }

    if (sat_r > 1) { sat_r = 1; }
    if (sat_g > 1) { sat_g = 1; }
    if (sat_b > 1) { sat_b = 1; }

    var result = new AmazonRGB(
        Math.round((1 - this.s + this.s * sat_r) * this.v * 255),
        Math.round((1 - this.s + this.s * sat_g) * this.v * 255),
        Math.round((1 - this.s + this.s * sat_b) * this.v * 255)
    );

    return result;
};


/* Container object that holds an RGB object and an HSV object.
   This is the catch-all type that gives you everything
   you will need up front.  With this object, you can create
   a color from RGB values, HSV values, or a hex string.

   Upon object creation, you are not guaranteed that any of the
   private properties exist yet (they are created as needed).
   Therefore, you must use accessor methods to get each property
   of this object.
*/
function AmazonColor(type, a, b, c)
{
    if (type == 'rgb')
    {
        this.rgb = new AmazonRGB(a,b,c);
        this.r = a;
        this.g = b;
        this.b = c;
    }
    else if (type == 'hsv')
    {
        this.hsv = new AmazonHSV(a,b,c);
        this.h = a;
        this.s = b;
        this.v = c;
    }
    else if (type.match(/[0-9a-f]{6}/i))
    {
        // 'type' argument was a hex color value
        var map = '0123456789ABCDEF';
        this.r = map.indexOf(type.substr(0,1)) * 16 + map.indexOf(type.substr(1,1));
        this.g = map.indexOf(type.substr(2,1)) * 16 + map.indexOf(type.substr(3,1));
        this.b = map.indexOf(type.substr(4,1)) * 16 + map.indexOf(type.substr(5,1));
        this.rgb = new AmazonRGB(this.r, this.g, this.b);
        this.hex = type;
    }
    else
    {
        // nothing to work with
        return null;
    }
}

// Accessor methods for properties.
// Requested properties are calculated when first requested if necessary,
// then stored for later.
AmazonColor.prototype.getR = function() {
    if (!this.r)
    {
        if (!this.rgb) { this.rgb = this.hsv.toRGB(); }
        this.r = this.rgb.getR();
    }
    return this.r;
};

AmazonColor.prototype.getG = function() {
    if (!this.g)
    {
        if (!this.rgb) { this.rgb = this.hsv.toRGB(); }
        this.g = this.rgb.getG();
    }
    return this.g;
};

AmazonColor.prototype.getB = function() {
    if (!this.b)
    {
        if (!this.rgb) { this.rgb = this.hsv.toRGB(); }
        this.b = this.rgb.getB();
    }
    return this.b;
};

AmazonColor.prototype.getH = function() {
    if (!this.h)
    {
        if (!this.hsv) { this.hsv = this.rgb.toHSV(); }
        this.h = this.hsv.getH();
    }
    return this.h;
};

AmazonColor.prototype.getS = function() {
    if (!this.s)
    {
        if (!this.hsv) { this.hsv = this.rgb.toHSV(); }
        this.s = this.hsv.getS();
    }
    return this.s;
};

AmazonColor.prototype.getV = function() {
    if (!this.v)
    {
        if (!this.hsv) { this.hsv = this.rgb.toHSV(); }
        this.v = this.hsv.getV();
    }
    return this.v;
};

AmazonColor.prototype.getHex = function() {
    if (!this.hex)
    {
        if (!this.rgb) { this.rgb = this.hsv.toRGB(); }
        this.hex = this.rgb.getHex();
    }
    return this.hex;
};

// Return a contrasting color of the same hue.
AmazonColor.prototype.getContrasting = function() {
    var newH = this.getH();
    var newS = this.getS();
    var newV = this.getV();

    if (newS == 0)
    {
        // Grayscale; don't change saturation
        newV = (newV + 0.5) % 1;
    }
    else
    {
        newS = (newS + 0.5) % 1;
        newV = (newV + 0.5) % 1;

        // Adjust color for better contrast if it's in the deep blue range.
        if ((newH > 200) && (newH < 275))
        {
            if ((newS >= 0.5) && (newV >= 0.5))
            {
                // High saturation and value, decrease saturation
                newS = newS / 2;
            }
            else if ((newS <= 0.5) && (newV <= 0.5))
            {
                // Low saturation and value, increase value
                newV = newV + 0.5;
            }
        }
    }

    return new AmazonColor('hsv', newH, newS, newV);
};

// Return a similar color of the same hue.
AmazonColor.prototype.getSimilar = function() {
    var newH = this.getH();
    var newS = this.getS();
    var newV = this.getV();

    // Change it more for highly-saturated colors.
    var tweak = (newS > 0.8) ? 0.1 : 0;

    if (newV > 0.6) { newV = newV - 0.2 - tweak; }
    else if (newV < 0.4) { newV = newV + 0.2 + tweak; }
    else { newV = newV - 0.3; }

    // Adjust color for better contrast if it's in the deep blue range.
    if ((newH > 200) && (newH < 275))
    {
        if (newS > 0.8) { newS = newS - 0.4; }
    }

    return new AmazonColor('hsv', newH, newS, newV);
};

// Simple test to see if a color is "dark" or not.
AmazonColor.prototype.isDark = function() {
    var rval = false;
    var v = this.getV();

    if (v < 0.6)
    {
        // Low value...it's dark.
        rval = true;
    }
    else
    {
        var h = this.getH();
        var s = this.getS();

        // A strongly-saturated blue or purple is considered "dark".
        if ((h > 200) && (h < 275))
        {
            if (s > 0.5) { rval = true; }
        }
    }

    return rval;
};



var oldErrorHandler = window.onerror;
window.onerror = amazon_error_handler;

// Set our defaults. 
if(!window.amazon_ad_width) { amazon_ad_width = 728; }
if(!window.amazon_ad_height) { amazon_ad_height = 90; }
if(!window.amazon_ad_tag) { amazon_ad_tag = ""; }
if(!window.amazon_ad_price) { amazon_ad_price = "all"; }
if(!window.amazon_ad_border) { amazon_ad_border = 'show'; }
if(!window.amazon_ad_logo) { amazon_ad_logo = 'show'; }
if(!window.amazon_ad_product_images) { amazon_ad_product_images = 'show'; }
if(!window.amazon_ad_link_target) { amazon_ad_link_target = "same"; }
if(!window.amazon_ad_referrer) { amazon_ad_referrer = "current"; }
if(!window.amazon_ad_discount) { amazon_ad_discount = "add"; }
if(!window.amazon_ad_linkcode) { amazon_ad_linkcode = "op1"; }

amazon_pvid();  

// Write the iframe
document.write('<ifr' + 'ame src="' + amazon_generate_url() + '" marginwidth="0" marginheight="0" width="' + amazon_ad_width + '" height="' + amazon_ad_height + '" border="0" frameborder="0" style="border:none;" scrolling="no"></iframe>');

// Flush parameters after render to restore defaults.
amazon_ad_width = null;
amazon_ad_height = null;
amazon_ad_price = null;
amazon_ad_border = null;
amazon_ad_logo = null;
amazon_ad_product_images = null;
amazon_ad_link_target = null;
amazon_ad_referrer = null;
amazon_ad_exclude = null;
amazon_ad_include = null;
amazon_ad_categories = null;
amazon_ad_discount = null;
amazon_ad_linkcode = null;
amazon_ad_title = null;

window.onerror = oldErrorHandler;

//-------------
// HELPER FUNCTIONS
//-------------
function amazon_generate_url()
{
  // Common stuff
  var amazon_q = window.location.protocol + "//" + amazon_ad_rcm + "/e/cm?";
  amazon_q += "t=" + amazon_ad_tag; 
  amazon_q += "&o=" + amazon_ad_o;
  amazon_q += "&p=" + amazon_p(amazon_ad_width,amazon_ad_height);
  amazon_q += "&l=" + amazon_ad_linkcode;
  amazon_q += "&pvid=" + amazon_ad_pvid;

  // Referring URL should be the parent location if the ad page is within an iframe.
  amazon_q += "&ref-url=" + escape((amazon_iframe() || (amazon_ad_referrer == 'parent')) ? document.referrer : document.location);
  amazon_q = amazon_q.replace(/\+/g,'%2B');

  amazon_q += "&ref-title=" + escape(document.title);

  // get referrer of the page serving these ads, if we can
  try{
    var referrer = top.document.referrer;
    amazon_q += "&ref-ref=" + escape(referrer);
  }
  catch(e)
    {}

  // Colors
  amazon_q+="&bgc="+amazon_fix_color(window.amazon_color_background, "FFFFFF");
  amazon_q+="&bdc="+amazon_fix_color(window.amazon_color_border, "000000");
  amazon_q+="&pcc="+amazon_fix_color(window.amazon_color_price, "990000");
  amazon_q+="&tec="+amazon_fix_color(window.amazon_color_text, "000000");
  amazon_q+="&tic="+amazon_fix_color(window.amazon_color_link, "3399FF");
  amazon_q+="&ac=" +amazon_fix_color(window.amazon_color_logo, "CC6600");

  var bgc = new AmazonColor(amazon_fix_color(window.amazon_color_background, "A1A1A1"));
  var pvc = bgc.getSimilar();
  amazon_q+="&pvc="+pvc.getHex();

  var bdc = new AmazonColor(amazon_fix_color(window.amazon_color_border, "000000"));
  if (bdc.isDark()) { amazon_q+="&lgl=1"; }

  // Display options
  if(amazon_ad_price=="all") {amazon_q+="&mp=1";}
  if(amazon_ad_border=='hide') {amazon_q+="&hb=1";}
  if(amazon_ad_logo=='hide') {amazon_q+="&hl=1";}
  if(amazon_ad_product_images=='hide') {amazon_q+="&hp=1";}
  if(amazon_ad_link_target=="new") {amazon_q+="&tg=_blank";}
  if(amazon_ad_discount=="add") {amazon_q+="&dsc=1";}

  // Advanced options
  if(window.amazon_ad_exclude)
  {
      amazon_q += '&exwords=' + filterKeywords(amazon_ad_exclude);
  }

  if(window.amazon_ad_include) 
  {
      amazon_q += '&inwords=' + filterKeywords(amazon_ad_include);
  }
  
  if(window.amazon_ad_categories) {amazon_q+='&incats='+amazon_ad_categories;}

  if(window.amazon_ad_title)
  {
      // Change all characters to their numerical forms for inclusion in a URI
      var codes = new Array;
      for (var i = 0; i < amazon_ad_title.length; ++i) { codes.push(amazon_ad_title.charCodeAt(i)); }
      var url_amazon_ad_title = codes.toString();
      amazon_q+='&title='+url_amazon_ad_title;
  }

  amazon_q += '&f=ifr';
  var e = (document.characterSet) ? document.characterSet : document.charset;
  if (e) { amazon_q += '&e='+e.toLowerCase(); }

  return amazon_q;
}

//takes a string keywords argument and makes sure it only contains
//the allowed number of phrases
function filterKeywords(keywords)
{
    var keyword_limit = 5;
    
    var rtn_keywords;
    if(keywords)
    {
        var items = keywords.split(/;/);
        if (items.length > keyword_limit)
        {
            items = items.slice(0,keyword_limit);
            rtn_keywords  = items.join(';');
        }
        else
        {
            rtn_keywords = keywords;
        }
    }
    
    return rtn_keywords;
}

function amazon_p(w, h)
{
  var p = new Array();
  p["120x150"] = 6;
  p["120x240"] = 8;
  p["180x150"] = 9;
  p["120x450"] = 10;
  p["120x600"] = 11;
  p["300x250"] = 12;
  p["468x60"] = 13;
  p["160x600"] = 14;
  p["468x240"] = 15;
  p["468x336"] = 16;
  p["600x520"] = 36;
  p["728x90"] = 48;
  
  return p[w + "x" + h];
}

function amazon_iframe()
{
  var rval = false;
  if (document.body)
  {
    rval = ((document.referrer != '') && ((document.body.clientWidth < (1.5 * amazon_ad_width)) && (document.body.clientHeight < (1.5 * amazon_ad_height))));
  }
  else if (document.documentElement)
  {
    rval = ((document.referrer != '') && ((document.documentElement.clientWidth < (1.5 * amazon_ad_width)) && (document.documentElement.clientHeight < (1.5 * amazon_ad_height))));
  }
  return rval;
}

function amazon_fix_color($color, $dflt)
{
  if(!$color) { return $dflt; }
  if($color.length != 6) { return $dflt; }
  $color = $color.toUpperCase();
  var $h2d = "0123456789ABCDEF";
  for(i = 0; i < 6; i++)
  {
    var $x = $color.charAt(i);
    if($h2d.indexOf($x) == -1 )
    {
      return $dflt;
    }
  }
  return $color;
}

function amazon_pvid()
{
  if(window.amazon_ad_pvid)
  {
    return amazon_ad_pvid;
  }

  amazon_ad_pvid = "";

  var $h2d = "0123456789ABCDEF";
  for(i = 0; i < 16; i++)
  {
    amazon_ad_pvid += $h2d.charAt(Math.floor( 16 * Math.random() ));
  }
}

function amazon_error_handler()
{
  return true;
}
