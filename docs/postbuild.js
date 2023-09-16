// this script is used to generate a file (with the current date)
// to inform that a new build is available
const fs = require('fs');
const filename = process.argv[2];
if (filename) {
  const date = new Date().toISOString();
  fs.writeFile(filename, date + '\n', (err) => { if (err) console.log(err); });
} else {
  console.log('specify filename as first command argument');
}
