import React, { Component } from "react";
import cloneDeep from "lodash/cloneDeep";
import Content from "./body/content.jsx";
class Body extends Component {
    constructor(props) {
        super(props);
        this.state = {content : {title:null}}
        //this.ref = React.createRef();
    }
    componentDidMount(){
        let active = cloneDeep(this.props.active);
        this.content(active, this.props.elements);
    }
    componentDidUpdate(){
        let active = cloneDeep(this.props.active);
        this.content(active, this.props.elements);
    }
    componentWillUnmount() {}
    content =(active, elements)=> {
        let index = active[0];
        if (active.length > 1) {
            let children = elements[index].children;
            active.shift();
            this.content(active, children);
        } else {
            let url =  elements[index].content;
            let content = require(`./body/contents/${url}`);
            
            if (content.title != this.state.content.title){
                this.setState({content});
            }
        }
    }
    fromContent = (e) => {
        if (e.link)
            this.props.onChange(e);
    }
    render() {
        let theme = this.props.theme ? "lightMode" : "darkMode";
        let left = this.props.left;
        let width = this.props.width;
        return (
            <div
                className= {`main docs body ${theme}`}
                style={{left, width}}>
                    <Content
                        theme = {theme}
                        index = {this.props.active}
                        content = {this.state.content}
                        onChange = {this.fromContent}
                    />
            </div>
        );
    }
}

export default Body;