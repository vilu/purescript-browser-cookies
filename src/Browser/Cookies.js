"use strict";

exports._setCookie = function _setCookie(cookie) {
  return function() {
    window.document.cookie = cookie;
    return {};
  };
};

exports._getCookies = function _getCookies() {
  return window.document.cookie;
};
