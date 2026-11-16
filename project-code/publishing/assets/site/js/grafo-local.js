(function () {
  function normalizar(caminho) {
    return decodeURI(caminho).replace(/\.html$/, '').replace(/\/$/, '');
  }

  function carregarD3(cb) {
    if (window.d3) { cb(); return; }
    var s = document.createElement('script');
    s.src = 'https://cdn.jsdelivr.net/npm/d3@7/dist/d3.min.js';
    s.onload = cb;
    document.head.appendChild(s);
  }

  function desenhar(svgEl, nodes, links, idAtual) {
    var svg = d3.select(svgEl);
    var largura = svgEl.clientWidth, altura = svgEl.clientHeight;
    var sim = d3.forceSimulation(nodes)
      .force('link', d3.forceLink(links).id(function (d) { return d.id; }).distance(70))
      .force('carga', d3.forceManyBody().strength(-180))
      .force('centro', d3.forceCenter(largura / 2, altura / 2))
      .force('colisao', d3.forceCollide().radius(16));

    var aresta = svg.append('g').selectAll('line').data(links).join('line');
    var no = svg.append('g').selectAll('g').data(nodes).join('g')
      .attr('class', function (d) {
        return 'no tipo-' + d.tipo + (d.id === idAtual ? ' atual' : '');
      });
    no.append('circle').attr('r', function (d) { return d.id === idAtual ? 9 : 6; });
    no.append('text').text(function (d) { return d.titulo; }).attr('x', 12).attr('y', 4);
    no.filter(function (d) { return d.id !== idAtual; })
      .on('click', function (e, d) { window.location.href = d.url; });

    sim.on('tick', function () {
      aresta.attr('x1', function (l) { return l.source.x; })
            .attr('y1', function (l) { return l.source.y; })
            .attr('x2', function (l) { return l.target.x; })
            .attr('y2', function (l) { return l.target.y; });
      no.attr('transform', function (d) {
        var x = Math.max(10, Math.min(largura - 10, d.x));
        var y = Math.max(10, Math.min(altura - 10, d.y));
        return 'translate(' + x + ',' + y + ')';
      });
    });
  }

  function iniciar() {
    var conteudo = document.querySelector('#content');
    if (!conteudo) return;
    var atualUrl = normalizar(location.pathname);

    fetch('/grafo.json').then(function (r) { return r.json(); }).then(function (dados) {
      var atual = dados.nodes.find(function (n) { return normalizar(n.url) === atualUrl; });
      if (!atual) return;

      var ids = new Set([atual.id]);
      dados.links.forEach(function (l) {
        if (l.source === atual.id) ids.add(l.target);
        if (l.target === atual.id) ids.add(l.source);
      });
      if (ids.size < 2) return;

      var nodes = dados.nodes
        .filter(function (n) { return ids.has(n.id); })
        .map(function (n) { return Object.assign({}, n); });
      var links = dados.links
        .filter(function (l) { return ids.has(l.source) && ids.has(l.target); })
        .map(function (l) { return { source: l.source, target: l.target }; });

      var secao = document.createElement('section');
      secao.id = 'grafo-local';
      secao.innerHTML = '<h2>Vizinhança</h2><svg role="img" aria-label="Notas ligadas a esta"></svg>';
      conteudo.appendChild(secao);
      carregarD3(function () { desenhar(secao.querySelector('svg'), nodes, links, atual.id); });
    }).catch(function () { /* sem grafo.json: ignora */ });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', iniciar);
  } else {
    iniciar();
  }
})();
