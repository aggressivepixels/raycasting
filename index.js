import { Elm } from "./src/Main.elm";

const DISPLAY_WIDTH = 256;
const DISPLAY_HEIGHT = 144;

const canvas = document.getElementById("canvas");
const app = Elm.Main.init();
app.ports.render.subscribe((pixels) => {
  const ctx = canvas.getContext("2d");
  const imageData = ctx.getImageData(0, 0, DISPLAY_WIDTH, DISPLAY_HEIGHT);
  for (let x = 0; x < DISPLAY_WIDTH; ++x) {
    for (let y = 0; y < DISPLAY_HEIGHT; ++y) {
      const index = (x + y * DISPLAY_WIDTH) * 4;
      imageData.data[index] = (pixels[x][y] & 0xff0000) >> 16;
      imageData.data[index + 1] = (pixels[x][y] & 0x00ff00) >> 8;
      imageData.data[index + 2] = pixels[x][y] & 0x0000ff;
      imageData.data[index + 3] = 255;
    }
  }

  ctx.putImageData(imageData, 0, 0);
});
