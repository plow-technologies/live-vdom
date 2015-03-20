angular.module('usernameLogin', [])

.directive('usernameLogin', function() {
  return {
    restrict: "E",
    replace: true,
    controller: ["$scope", "$http", "$window", function($scope, $http, $window) {

      var hideErrorMessage = function() {
        $scope.isErrorMessageVisible = false;
      };

      var hideSuccessMessage = function() {
        $scope.isSuccessMessageVisible = false;
      };

      var submit = function() {

        var loginRequest = {
          username: $scope.username,
          password: $scope.password
        }

        $http.post('/auth/page/plow/login', loginRequest)
          .success(function(data, status) {
            if(status === 200) {
              $scope.isSuccessMessageVisible = true;
              $window.location = "/";
            }
          })
          .error(function(data, status) {
            if(status === 401) {
              $scope.isErrorMessageVisible = true;
            }
          })
      };

      // Initialization
      $scope.isErrorMessageVisible = false;
      $scope.isSuccessMessageVisible = false;

      // Exposed API
      $scope.submit = submit;
      $scope.hideErrorMessage = hideErrorMessage;
      $scope.hideSuccessMessage = hideSuccessMessage;
    }],
    templateUrl: "/static/angular/templates/usernameLogin.html"
  };
});
