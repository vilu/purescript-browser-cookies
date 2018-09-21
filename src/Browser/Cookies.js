"use strict";

exports._setCookie = function _setCookie(cookie) {
    return function() {
        document.cookie = cookie;
        return {};
    }
};

exports._getCookies = function _getCookies() {
    return document.cookie;
};
