
// TODO I should probably use a normal build process for this. It's weird to import it separately, but whatever.

console.log("Index", React)

// It's already working, only updating the relevant parts

class App extends React.Component {

  constructor(props) {
    super(props);
    this.state = {html: props.html}
  }

  setHtml(html) {
    this.setState({html: html})
  }

  // ready to go
  componentDidMount() {
    //listen for link click events at the document level
    if (document.addEventListener) {
      document.addEventListener('click', this.onClick.bind(this));
      document.addEventListener('submit', this.onSubmit.bind(this));

      // do I want to do this? Or just to the ones that have it?
      // document.addEventListener('change', onInput);
    } else if (document.attachEvent) {
        document.attachEvent('onclick', this.onClick.bind(this));
        document.attachEvent('onsubmit', this.onSubmit.bind(this));
      // document.attachEven('onchange', onInput);
    }

    // support history
    window.addEventListener('popstate', (event) => {
      // console.log("location:", document.location, "state:", event.state)
      console.log("popstate", event.state, event.state.pageUrl)
      if (event.state.pageUrl) {
        this.restore(event.state.pageUrl)
      }
    })
  }

  runtime(action) {
    console.log(action)
    let body = messageBody(action)
    sendMessage(body)
      .then(onResponse)
      .then(this.onResponseBody.bind(this))
  }

  // when you click a link
  link(url) {
    sendLoad(url)
      .then(onResponse)
      .then(this.onResponseBody.bind(this))
  }

  restore(url) {
    // same as above, but don't push state
    sendLoad(url)
      .then(res => res.text())
      .then(this.onResponseBody.bind(this))
  }



  onResponseBody(body) {
    console.log("BODY", body)
    this.setState({html: body})
  }


  onClick(e) {
    // check for our clicks
    var click = e.target.dataset.click;
    if (click) {
      this.runtime(click)
    }

    else {
      this.interceptClickEvent(e)
    }
  }

  interceptClickEvent(e) {
      // Check for intercepted links
      var href;
      var target = e.target || e.srcElement;
      if (target.tagName === 'A') {
          href = target.getAttribute('href');

          // TODO this shouldn't be hard coded. how do we know it's an app request? Can we assume any local request goes to the app server? Any link click? Probably. What about a static page?
          // HMM... we probably need to send down some constants here
          if (href.startsWith("/app")) {
            console.log("INTERCEPT LINK", href)
            link(href)
            e.preventDefault();
          }
      }
  }



  onSubmit(e) {
    console.log("ONSUBMIT", e.target.dataset)
    e.preventDefault();

    var form = e.target
    var submit = form.dataset.submit
    var submit1 = form.dataset.submit1
    var inputs = "input,textarea"

    if (submit) {

      // find any named inputs and textareas and add them
      var formData = {}
      for (var child of form.querySelectorAll(inputs)) {
        if (child.name) {
          formData[child.name] = child.value
        }
        else if (child.id) {
          formData[child.id] = child.value
        }
      }

      var action = submit + " " + encodeMap(formData)
      console.log("Submit", action)
      this.runtime(action)
    }
    else if (submit1) {

      // collect the value from the first input or textarea we find
      var child = form.querySelector(inputs)
      var value = child.value || ""
      var action = submit1 + " " + JSON.stringify(value)
      console.log("Submit1", action)
      this.runtime(action)
    }
  }

  // it calls immediately, but doesn't update the value for some reason?
  // it works, but e.target.value changes?
  // it's not returning true or something...
  // or it's re-asserting the value from react
  onInput(e) {
    console.log("INPUT", e.target.value)
    var target = e.target
    var onInput = target.dataset.input
    if (onInput) {
      var value = target.value || ""
      var action = onInput + " " + JSON.stringify(value)
      this.runtime(action)
    }
  }

  render() {
    var content = parseServerHTML(this.state.html)
    return content
  }
}


// the root application
var root;



function init() {
  console.log("INIT")

  // not working very well, because it's not "managed"
  // Write default value, which only has to happen on load
  var defaultValues = document.querySelectorAll('[data-default-value]').forEach(function(el) {
    el.value = el.dataset.defaultValue
  })

  // Hydrate with the actual contents
  var content = document.getElementById("content").innerHTML;
  console.log("CONTENT", content)
  // var el = parseServerHTML(content)

  // ReactDOM.hydrate(el, document.getElementById('content'))
  root = React.createElement(App, {html:content})
  ReactDOM.hydrate(root, document.getElementById('content'))


}

function sendMessage(body) {
  return fetch(location.href, {
    method: "POST",
    headers: {"Accept": "application/vdom"},
    body: body
  })
}


function sendLoad(url) {
  return fetch(url, {
    method: "GET",
    headers: {"Accept": "application/vdom"}
  })
}

function messageBody(action) {
  return action
}

function onResponse(res) {
    let pageUrl = res.headers.get('X-Page-Url')

    // TODO titles
    let title = "Wookie Tab Title"
    console.log(pageUrl)
    window.history.pushState({pageUrl: pageUrl}, title, pageUrl)
    return res.text()
  }





function testInput(e) {
  console.log("TEST INPUT", e.target.value)
}




function encodeMap(obj) {
  var pairs = []
  for (var key in obj) {
    pairs.push([key, obj[key]])
  }

  return "(fromList [" + pairs.map(encodePair).join(",") + "])"
}

function encodePair(pair) {
  return "(" + JSON.stringify(pair[0]) + "," + JSON.stringify(pair[1]) + ")"
}


// wow, onInput is total garbage
// because it.... calls the server immediately, and then replaces the field with what the server returns. I need a debounced version
function parseServerHTML(html) {
  return HTMLReactParser(html)
  // return HTMLReactParser(html, {replace: function(domNode) {
  //   // if it has data-input at all, add an onInput handler
  //   if (domNode.attribs && domNode.attribs.defaultvalue) {
  //     // console.log("DOM NODE", domNode.attribs)
  //     // domNode.attribs.defaultValue = domNode.attribs.defaultvalue
  //     // delete domNode.attribs.defaultvalue
  //     console.log("DOM NODE", domNode.attribs)
  //     domNode.attribs.defaultValue = domNode.attribs.defaultvalue

  //     // so the server is sending down the "defaultvalue" attribute
  //     // which it doesn't like
  //     // I could go through and replace them all
  //     // this is stupid and annoying
  //     // but if I set value it'll yell at me too
  //     delete domNode.attribs.defaultvalue

  //   //   // so, the problem is, it hits, but it doesn't update the value
  //   //   // so debouncing won't work
  //   //   // just don't do this...
  //   //   // i think, just don't do this
  //   //   // only use react to render
  //   //   domNode.attribs.onInput = function(e) {
  //   //     console.log("ON INPUT", e.target.value)
  //   //   }

  //   //   // domNode.attribs.onInput = "testInput"
  //   //     // debounce(testInput, 250)

  //   // //   delete domNode.attribs.oninput
  //   // //   domNode.attribs.onInput = inp
  //     // return HTMLReactParser.domToReact(domNode)
  //     // delete domNode.attribs.defaultvalue
  //     return HTMLReactParser.domToReact(domNode)
  //   }
  // }})
}

init();







// Returns a function, that, as long as it continues to be invoked, will not
// be triggered. The function will be called after it stops being called for
// N milliseconds. If `immediate` is passed, trigger the function on the
// leading edge, instead of the trailing.
function debounce(func, wait, immediate) {
	var timeout;
	return function() {
		var context = this, args = arguments;
    console.log("DEBOUNCE", args[0].target.value)
		var later = function() {
      console.log(" - later", args[0].target.value)
			timeout = null;
			if (!immediate) func.apply(context, args);
		};
		var callNow = immediate && !timeout;
		clearTimeout(timeout);
		timeout = setTimeout(later, wait);
		if (callNow) {
      console.log("CALL NOW")
      func.apply(context, args);
    }
	};
};
