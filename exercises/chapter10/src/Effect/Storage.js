"use strict";

exports.setItem = (key) => (value) => () =>
  window.localStorage.setItem(key, value);

exports.getItem = (key) => () => window.localStorage.getItem(key);

// (Easy) Write a wrapper for the removeItem method on the localStorage object, and
// add your foreign function to the Effect.Storage module.

exports.removeItem = (key) => () => window.localStorage.removeItem(key);

// (Easy) Write a wrapper for the confirm method on the JavaScript Window object,
// and add your foreign function to the Effect.Alert module.

exports.confirm = (string) => () => window.confirm(string);
