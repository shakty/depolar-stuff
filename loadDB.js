const NDDB = require('NDDB').NDDB;
const path = require('path');
const J = require('JSUS').JSUS;
const fs = require('fs');

const THISDIR = process.platform === 'linux' ?
      '/home/balistef/Dropbox/msr-ineq/R' :
      'C:\\Users\\stbaliet\\Dropbox\\msr-ineq\\R';

const SESSIONDIR = 'DATA2';
const SESSIONNAME = 'pilot2-cons';
const SESSIONDATE = '04-25-2019';


const DATADIR = path.join(THISDIR, SESSIONDIR);

let db = new NDDB({ update: { indexes: true }});
db.hash('player');
db.view('essay');


// Load.
db.loadSync('db.json');

let db2 = new NDDB();
console.log(db.size());
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
    db2.insert({
        player: essay.player,
        essayTime: (essay.time / 1000),
        essayWords: essay.essay.split(" ").length,
        overallTime: overallTime / 1000,
        consentTime: first.time / 1000,
        session: SESSIONNAME,
        date: SESSIONDATE
    });
});

console.log(db2.size());
db2.save('from_json.csv');

