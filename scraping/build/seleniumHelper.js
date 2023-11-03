"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const { Builder, Capabilities, By, Key, until, WebDriver } = require('selenium-webdriver');
function simpleTest() {
    return __awaiter(this, void 0, void 0, function* () {
        const driver = yield new Builder()
            .forBrowser('chrome')
            .usingServer("http/localhost:4444&wd/hub")
            .build();
        try {
            yield driver.get('https://google.com');
            const title = yield driver.getTitle();
            console.log('Title:', title);
        }
        finally {
            yield driver.quit();
        }
    });
}
exports.default = simpleTest;
