const NDDB = require('NDDB').NDDB;
const path = require('path');
const J = require('JSUS').JSUS;
const fs = require('fs');

const THISDIR = process.platform === 'linux' ?
      '/home/balistef/Dropbox/msr-ineq/R' :
      'C:\\Users\\stbaliet\\Dropbox\\msr-ineq\\R';

var SESSIONDIR = 'DATA2';
var SESSIONNAME = 'pilot2-cons';
var SESSIONDATE = '04-25-2019';

var SESSIONDIR = 'DATA';
var SESSIONNAME = 'pilot1';
var SESSIONDATE = '04-22-2019';


const DATADIR = path.join(THISDIR, SESSIONDIR);


let db = new NDDB({ update: { indexes: true }});
db.hash('player');
db.view('essay');

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
    db.save(path.join(DATADIR, 'db.json'));
    processIt();
});

setTimeout(() => { asyncQueue.remove('wait_a_little'); }, 1000);


function processIt() {    
    let db2 = new NDDB();
    // console.log(db.size());
    db.essay.each((essay) => {
        // This guy disconnected and reconnected with the answer.
        if (essay.player === 'uVXfKf37') {
            let commit = db.select('player', '=', 'uVXfKf37')
                .and('commit')
                .fetch()[0];
            let time = essay.timestamp - commit.timestamp;
            essay.time = time;
        }
        // Get overall time.
        let first = db.player[essay.player].first();
        let last = db.player[essay.player].select('feedback').fetch()[0];
        let overallTime = (last.timestamp - first.timestamp) + first.time;
        let item = {
            player: essay.player,
            essayTime: (essay.time / 1000),
            essayWords: essay.essay.split(" ").length,
            overallTime: overallTime / 1000,
            consentTime: first.time / 1000,
            session: SESSIONNAME,
            date: SESSIONDATE
        };
        db2.insert(item);
    });

    console.log(db2.size());
    db2.save(path.join(DATADIR, 'from_json.csv'));
}

// Middle value=4, 7 answers = 28;
// Positive values: pro redistribution.
function computeStance(p, r) {
    r = r || '';
    debugger
    return (p['govred' + r] +
            p['estatetax' + r] +
            p['billionaires' + r] + 
            p['aidpoor' + r] +
            p['minimalwage' + r] +
            p['publichousing' + r] +
            p['foodstamps' + r] - 28);
}
