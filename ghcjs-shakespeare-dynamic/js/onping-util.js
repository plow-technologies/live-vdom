//swapArray takes an array and changes the position of two elements
// It returns a new unlinked array and also swaps the original
function swapArray(arr, i, j) {
    "use strict";
    if (arr[i] && arr[j]) {
        var t = arr[i];
        arr[i] = arr[j];
        arr[j] = t;
    }
    return arr.slice(); //generate unlinked array
}

function addUnchecked(data) {
    for (var i = 0; i < data.length; ++i) {
        data[i].style = "fa-icon-check-empty";
    }
}


function clone(src) {
    "use strict";

    function mixin(dest, source, copyFunc) {

        var name, s, empty = {};
        for (name in source) {
            // the (!(name in empty) || empty[name] !== s) condition avoids copying properties in "source"
            // inherited from Object.prototype.      For example, if dest has a custom toString() method,
            // don't overwrite it with the toString() method that source inherited from Object.prototype
            if ('true') {
                s = source[name];
                if (!(name in dest) || (dest[name] !== s && (!(name in empty) || empty[name] !== s))) {
                    dest[name] = copyFunc ? copyFunc(s) : s;
                }
            }
        }
        return dest;
    }

    if (!src || typeof src !== "object" || Object.prototype.toString.call(src) === "[object Function]") {
        // null, undefined, any non-object, or function
        return src; // anything
    }
    if (src.nodeType && "cloneNode" in src) {
        // DOM Node
        return src.cloneNode(true); // Node
    }
    if (src instanceof Date) {
        // Date
        return new Date(src.getTime()); // Date
    }
    if (src instanceof RegExp) {
        // RegExp
        return new RegExp(src); // RegExp
    }
    var r, i, l;
    if (src instanceof Array) {
        // array
        r = [];
        for (i = 0, l = src.length; i < l; ++i) {
            if (i in src) {
                r.push(clone(src[i]));
            }
        }
        // we don't clone functions for performance reasons
        //              }else if(d.isFunction(src)){
        //                      // function
        //                      r = function(){ return src.apply(this, arguments); };
    } else {
        // generic objects
        r = src.constructor ? new src.constructor() : {};
    }
    return mixin(r, src, clone);

}


function getWith(fGet, arr) {
    "use strict";
    // Would love to contract no side effect in the get 
    // but can't, so don't do it!

    var test = null;
    for (var i = 0; i < arr.length; ++i) {
        test = arr[i];
        if (fGet(test)) {
            return test;
        }
    }
    return null;
}



//**************************************************
// Configuration Defaults 
//**************************************************

function defaultDisplayToggle() {
    "use strict";
    //makes a function for handling the min max of a widget
    var f = function(data) {
        if (data.hasOwnProperty("minstate")) {
            if (data.minstate === '' || data.minstate === true) {
                data.minstate = true;
            } else {
                data.minstate = false;
            }


        }
    };
    return f;
}


function defaultShowHideMenu(scope) {
    "use strict";
    var shM = function() {
        if (scope.hide) {
            scope.hide = false;
        } else {
            scope.hide = true;
        }

    };
    return shM;
}

function defaultShowPhoneMenu(scope) {
    "use strict";
    var dsPM = function() {
        if (scope.pmenuStyle.height === "0") {
            scope.pmenuStyle = {
                height: "auto"
            };
        } else {
            scope.pmenuStyle = {
                height: "0"
            };
        }
    };
    return dsPM;
}

function defaultNewWidget() {
    var newWidget = {};
    newWidget.size = {};
    newWidget.size.size = 12;
    newWidget.minstate = false;
    newWidget.edistate = false;
    newWidget.remove = false;
    return newWidget;
}

// resolve Singleton makes an object an array or just returns the array if the 
//object already is one, intended for use with select-2
function resolveSingleton(sOrArr) {
    if (Array.isArray(sOrArr)) {
        return sOrArr;
    } else {
        return [sOrArr];
    }
    //Type correctness!
}

//default inc and dec functions for box widgets;
function defltInc(data) {
    if (data.hasOwnProperty("size")) {
        if (data.size.hasOwnProperty("size")) {
            data.size.size = data.size.size + 1;
        }
    }
}

function defltDec(data) {
    if (data.hasOwnProperty("size")) {
        if (data.size.hasOwnProperty("size")) {
            data.size.size = data.size.size - 1;
        }
    }
}



//Create a default Box widget size
var makeDefaultPageWidget = function(title) {
    var exBoxConfig = {};

    exBoxConfig.title = title;
    exBoxConfig.icon = 'icon-user';
    exBoxConfig.size = {
        size: '4',
        inc: defltInc,
        dec: defltDec
    };
    exBoxConfig.maxVsize = 400;
    exBoxConfig.minstate = false;
    exBoxConfig.editstate = false;
    exBoxConfig.remove = false;
    exBoxConfig.controls = [];
    return exBoxConfig;
};



//Object Copy Correct!!
function objectCopy(items) {
    var newItems = {};

    for (var item in items) {
        if (items.hasOwnProperty(item)) {
            newItems[item] = items[item];
        }
    }
    return newItems;
}


//
function getRawParams(p) {
    for (var i in p) {
        if (p.hasOwnProperty(i)) {
            return p[i];
        }
    }
}

// takes the current day and the target week day and returns the date of that day
// the currDay is of type Date () the targetDay is an int 0 = Sunday <--> 6 = Saturday

function getNearestWeekDay(currDate, targetDay) {
    var dint = currDate.valueOf();
    var day = currDate.getDay();
    var diffStd = targetDay - day;
    var diffMod = Math.abs((day + diffStd)) % 7;
    var diffUsed = 0;
    var diffInMs = 0;
    if (diffStd < diffMod) {
        diffUsed = diffStd;

    } else {
        diffUsed = diffMod;
    }


    diffInMs = diffUsed * 24 * 3600 * 1000;
    return (new Date(dint + diffInMs));


}

// way to use css in the body tag
function loadCSS(filename) {
    "use strict";
    var file = document.createElement("link")
    file.setAttribute("rel", "stylesheet")
    file.setAttribute("type", "text/css")
    file.setAttribute("href", filename)

    if (typeof file !== "undefined")
        document.getElementsByTagName("head")[0].appendChild(file)
}

/* Setup the information for mobile switching */
function iBuildShowPhoneMenu() {
    var scope = {};
    scope.tuple = "Menu";
    scope.hide = 0;
    scope.pmenuStyle = {
        height: "0"
    };
    scope.showPhoneMenu = function() {
        if (this.pmenuStyle.height === "0") {
            this.pmenuStyle = {
                height: "auto"
            };
        } else {
            this.pmenuStyle = {
                height: "0"
            };
        }
    };
    return scope;
}
