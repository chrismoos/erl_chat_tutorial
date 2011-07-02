var msgID = 0;
function addHTML(HTML) {
	var element = document.getElementById("html_box");
	element.appendChild(HTML);
	scrollToBottom("html_box");
}

function addText(Text) {
	var txt = document.createElement("div");
	txt.innerHTML = Text;
	document.getElementById("html_box").appendChild(txt);
	scrollToBottom("html_box");
}

function addSystemMsg(Msg) {
	addText('<span style="color:black;">*** ' + Msg + '</span>');
}

function scrollToBottom(elm_id)
	{
	var elm = document.getElementById(elm_id);
	try
		{
		elm.scrollTop = elm.scrollHeight;
		}
	catch(e)
		{
		var f = document.createElement("input");
		if (f.setAttribute) f.setAttribute("type","text")
		if (elm.appendChild) elm.appendChild(f);
		f.style.width = "0px";
		f.style.height = "0px";
		if (f.focus) f.focus();
		if (elm.removeChild) elm.removeChild(f);
		}
	}

function onleave() {
    
}

function sendChatMessage(msg) {
	new Ajax.Request('/chat/send_msg/',
	  {
	    method:'post',
	    parameters: {msg: msg},
	    onSuccess: function(transport){
	    },
	    onFailure: function(){ }
	  });
}


function onTextEnter(e) {
	var characterCode;

	if(e && e.which) {
		e = e;
		characterCode = e.which;
	}		
	else{
		e = event;
		characterCode = e.keyCode;
	}

	if(characterCode == 13) { 
		
		var element = document.getElementById("chat_txt");
		
		sendChatMessage(element.value);
		
		element.value = "";
		return false;
	}
	else{
		return true;
	}
}

function addToUsers(HTML) {
	var element = document.getElementById("users");
	element.appendChild(HTML);
}


function addUser(Nick) {
	var txt = document.createElement("div");
	txt.id = "users_" + Nick.escapeHTML();
	txt.style.width = "90%";
	txt.style.padding = "5px";
	txt.innerHTML = '<span style="border-bottom: 1px solid black; padding-top: 5px; padding-left: 5px; color: blue;">' + Nick.escapeHTML() + '</span>';
	addToUsers(txt);
}

function logout() {
	new Ajax.Request('/chat/leave/',
	  {
	    method:'get',
	    onSuccess: function(transport){
	        window.location='/';
	    },
	    onFailure: function(){ }
	  });
}

function removeUser(Nick) {
	var u = document.getElementById("users_" + Nick.escapeHTML());
	document.getElementById("users").removeChild(u);
}

function getServiceMsg() {
	new Ajax.Request('/chat/wait/?msg_id=' + msgID,
	  {
	    method:'get',
	    onSuccess: function(transport){
	        if(transport.responseText == "") {
	            setTimeout('getServiceMsg();', 10000);
	        }
	        else {
	            var response = transport.responseText.evalJSON();
	            if(handleServiceMsg(response) == true) {
		  	        getServiceMsg();
		        }
	        }
	    },
	    onFailure: function(){ 
	        setTimeout('getServiceMsg();', 10000) }
	  });
}

function updateMsgID(x) {
	if (x >= msgID) {
		msgID = x + 1;
	}
}

function setChatMsg(m) {
	document.getElementById("chat_txt").value = m;
	document.getElementById("chat_txt").focus();
}

function handleServiceMsg(response) {
    
	if(response.status == "ok") {
		if(response.response == "reconnect") return true;
		for(var x = 0; x < response.response.length; x++) {
			var data = response.response[x].d;
			var id = response.response[x].id;
			var t = response.response[x].t;
			
			updateMsgID(id);
			
			if(t == "user_joined_room") {
				addSystemMsg(data.escapeHTML() + " has joined the room.");
				addUser(data.escapeHTML());
			}
			else if(t == "user_left_room") {
				addSystemMsg(data.nick.escapeHTML() + " has left the room(" + data.reason.escapeHTML() + ")");
				removeUser(data.nick.escapeHTML());
			}
			else if(t == "chat_msg"){
				addText('<span style="color:red;">' + data.nick.escapeHTML() + "</span>: " + data.msg.escapeHTML());
			}
			else if(t == "sent_chat_msg"){
				addText('<span style="color:blue;">' + data.nick.escapeHTML() + "</span>: " + data.msg.escapeHTML());
			}
			else if(t == "system_msg") {
			    addSystemMsg(data);
			}
		}
		return true;
	}
	else if(response.status == "error") {
	 	if(response.response == "bad_session") addSystemMsg("Your session is no longer valid. Please <a href=\"/\">login</a> again.");
		else addSystemMsg(response.response);
		return false;
	}
	else {
		addSystemMsg("Received an invalid response from the server. Please refresh this page.");
		return false;
	}
}

function getOnlineUsers() {
    new Ajax.Request('/chat/online/',
	  {
	    method:'get',
	    onSuccess: function(transport){
			var response = transport.responseText.evalJSON();
			
			if(response.status == "ok") {
				for(var x = 0; x < response.response.length; x++) addUser(response.response[x]);
				getServiceMsg();
			}
			else {
			    addSystemMsg("Unable to access user list at this time.");
			}
	    },
	    onFailure: function(){ addSystemMsg("The chat service is not responding at this time.") }
	  });
}

function startClient() {
	addSystemMsg("Establishing connection to chat service...");
	new Ajax.Request('/chat/start/',
	  {
	    method:'get',
	    onSuccess: function(transport){
			var response = transport.responseText.evalJSON();
			
			if(response.status == "ok") {
				msgID = response.response;
				addSystemMsg("Connected to chat service.");
				getOnlineUsers();
			}
			else if(response.status == "error" && response.response == "bad_session") addSystemMsg("Unable to locate your session. Please <a href=\"/\">login</a> again.");
	    },
	    onFailure: function(){ addSystemMsg("The chat service is not responding at this time.") }
	  });
}