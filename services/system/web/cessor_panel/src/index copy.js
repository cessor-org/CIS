require("file-loader?name=[name].[ext]!./index.html")
import React from 'react';
import ReactDOM from 'react-dom';
import App from './App';

//  style
import './index.css';
import './App.css';
//import "bootstrap/dist/css/bootstrap.css";
import 'bootstrap/dist/css/bootstrap.min.css';

ReactDOM.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
  //, document.getElementById('root')
);
