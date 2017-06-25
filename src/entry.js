let entry = require('./Main.purs');
let htmlCanvas = document.getElementById('viewport')
htmlCanvas.width = window.innerWidth;
htmlCanvas.height = window.innerHeight;
let app = entry.main()
