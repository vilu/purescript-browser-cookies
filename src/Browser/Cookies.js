"use strict";

exports._setCookie = function _setCookie(cookie) {
    document.cookie = cookie;
};

exports._getCookies = function _getCookies() {
    return document.cookie;
};
