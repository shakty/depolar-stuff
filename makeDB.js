const NDDB = require('NDDB').NDDB;
const path = require('path');
const J = require('JSUS').JSUS;
const fs = require('fs');

const THISDIR = process.platform === 'linux' ?
      '/home/balistef/Dropbox/msr-ineq/R' :
      'C:\\Users\\stbaliet\\Dropbox\\msr-ineq\\R';

const DATADIR = path.join(THISDIR, 'DATA');


let db = new NDDB({ update: { indexes: true }});
let asyncQueue = J.getQueue();
let loaded = 0;

asyncQueue.add('wait_a_little');

fs.readdir(DATADIR, function(err, files) {
    if (err) throw(err);

    J.each(files, (f) => {
        let gameDir = path.join(DATADIR, f);
        fs.stat(gameDir, function(err, stats) {
            if (err) throw(err);
            if (!stats.isDirectory()) return;
            let dbfile = path.join(gameDir, 'memory_all.json');
            if (!fs.existsSync(dbfile)) {
                console.log(gameDir + ' has no file');
                return;
            }
            asyncQueue.add(dbfile);
            db.loadSync(dbfile, () => {
                loaded++;
                asyncQueue.remove(dbfile);
            });
        });
    });
});

asyncQueue.onReady(function() {
    console.log('Loaded: ' + loaded);
    console.log('Items: ' + db.size());
    db.save('db.json');
});

setTimeout(() => { asyncQueue.remove('wait_a_little'); }, 1000);
