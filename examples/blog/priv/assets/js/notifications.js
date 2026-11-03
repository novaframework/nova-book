const list = document.getElementById('notifications');

function describe({ event, data }) {
  switch (event) {
    case 'post_published': return `New post: ${data.title}`;
    case 'comment_created': return `${data.author} commented on ${data.post_title}`;
    default: return event;
  }
}

function connect(attempt = 0) {
  const scheme = location.protocol === 'https:' ? 'wss' : 'ws';
  const socket = new WebSocket(`${scheme}://${location.host}/ws/notifications`);
  const keepAlive = setInterval(() => socket.send('ping'), 30000);
  socket.onopen = () => { attempt = 0; list.replaceChildren(); };
  socket.onmessage = (message) => {
    if (message.data === 'pong') return;
    const item = document.createElement('li');
    item.textContent = describe(JSON.parse(message.data));
    list.prepend(item);
    while (list.children.length > 5) list.lastElementChild.remove();
  };
  socket.onclose = () => {
    clearInterval(keepAlive);
    setTimeout(() => connect(attempt + 1), Math.min(1000 * 2 ** attempt, 30000));
  };
}

if (list) connect();
