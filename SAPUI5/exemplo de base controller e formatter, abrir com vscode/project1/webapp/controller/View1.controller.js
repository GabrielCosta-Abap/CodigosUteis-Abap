sap.ui.define([
	"project1/controller/BaseController",
	"project1/model/formatter"
], function(BaseController, formatter) {
	"use strict";
	return BaseController.extend("project1.controller.Produtos", {
		onInit: function() {
			this.sayHi()
		},

		onNavBack: function() {

		}

	});

});