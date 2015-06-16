(function(angular) {
  'use strict';
angular.module('ghcjsApp', [])
  .directive('ghcjsDirective', function() {
    return {
      scope: {
        handler: '@handler', // Handler is passed in as a string
        config: '='          // Config is passed in as a JSObject
      },
      link: function (scope, element) {
        function whenExists() {
            window.setTimeout(function() {
                if (typeof window[scope.handler] != "undefined" 
                      && typeof scope.config != "undefined") { //Check for the config and handler function
                                                              // The handler function must be global
                    window[scope.handler](element[0], scope.config); //Pass the domnode and the config into the handler
                } else {
                    arguments.callee();
                }
            }, 10); // Wait for the function to exist
        }
        whenExists(); // Run the check for handler and config
      }
    };
  });
})(window.angular);