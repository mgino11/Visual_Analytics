function csvParseAsTable(csv) {
    var table_body = d3.csvParseRows(csv, function (d, i) {
        var r = ['<tr>'];
        if (i == 0) {
            // process header row
            r.push('<th>#</th>');
            for (const x in d) {
                r.push(`<th>${d[x]}</th>`);
            }
        }
        else {
            // process data row
            r.push(`<td>${i}</td>`);
            for (const x in d) {
                r.push(`<td>${d[x]}</td>`);
            }
        }
        r.push('</tr>');
        return r.join('');
    });
    return `<table>${table_body.join('')}</table>`;
}