function reverseText(txt) {
    if (txt == undefined || txt.length == 0) {
        return txt;
    }
    var a = Array.from(txt);
    var len = txt.length
    for (let i = 0; i < a.length / 2; i++) {
        const c = a[i];
        a[i] = a[len - i - 1];
        a[len - i - 1] = c;
    }
    return a.join('');
}

function docWriteMulti20Table(txt) {
    const i = +txt;
    var t = txt.length == 0 ? '' : `<table>\
    <tr><td>${1 * i}</td><td>${2 * i}</td><td>${3 * i}</td><td>${4 * i}</td></tr>\
    <tr><td>${5 * i}</td><td>${6 * i}</td><td>${7 * i}</td><td>${8 * i}</td></tr>\
    <tr><td>${9 * i}</td><td>${10 * i}</td><td>${11 * i}</td><td>${12 * i}</td></tr>\
    <tr><td>${13 * i}</td><td>${14 * i}</td><td>${15 * i}</td><td>${16 * i}</td></tr>\
    <tr><td>${17 * i}</td><td>${18 * i}</td><td>${19 * i}</td><td>${20 * i}</td></tr>\
    </table>`;
    return t;
}