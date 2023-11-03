import { Builder, By, WebDriver, WebElement, until } from "selenium-webdriver";
const {Options} = require('selenium-webdriver/chrome');

export default async function simpleTest() {

    let options = new Options();
    options.addArguments('--no-sandbox');
    options.addArguments('--disable-dev-shm-usage');

    const driver: WebDriver = await new Builder()
    .forBrowser('chrome')
    .setChromeOptions(options)
    .usingServer("http://localhost:4444/wd/hub")
    .build();

    try {
        await driver.get('https://badmintonplayer.dk/DBF/se-hvor-du-kan-spille-badminton/#1,,,,,0,,');
        driver.wait(until.elementsLocated(By.css('#ctl00_ContentPlaceHolder1_ShowClub1_PanelSearchResult > table > tbody > tr > td > a')));
        let clubs = await driver.findElements(By.css('#ctl00_ContentPlaceHolder1_ShowClub1_PanelSearchResult > table > tbody > tr > td > a'));
        console.log(`Count: ${clubs.length}`);
        clubs.map(async c => await c.getText())
            .forEach(x => console.log(x));
    } finally {
        await driver.quit();
    }
}