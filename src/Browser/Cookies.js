"use strict";

export const _setCookie = function _setCookie(cookie) {
  return function() {
    window.document.cookie = cookie;
    return {};
  };
};

export const _getCookies = function _getCookies() {
  return window.document.cookie;
};
