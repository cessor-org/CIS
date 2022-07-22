import React, { Component } from "react";
import { cloneDeep } from 'lodash';
import MarkdownView from 'react-showdown';

class Body extends Component {
    constructor(props) {
        super(props);
        this.state = {
            markdown : null,
            active_md : null
        }
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
    content =async (active, elements)=> {
        let index = active[0];
        if (active.length > 1) {
            let children = elements[index].children;
            active.shift();
            this.content(active, children);
        } else {
            let mdFile =  elements[index].content;
            let URL = require(`./body/contents/${mdFile}`);
            
            if (mdFile !== this.state.active_md){
                let text = await this.fetchDock(URL);
                let element = cloneDeep(elements[index]);
                let mdRaw = this.contentRef(element, text);
                this.setState({
                    markdown: mdRaw,
                    active_md: mdFile
                });
            }
            
        }
    }
    fetchDock = async (URL) => {
        try {
            let res = await fetch(URL);
            return await res.text();
        } catch (error) {
            console.log(error);
        }
    }
    contentRef = (element, mdRaw) => {
        if (element.ref.length === 0) {
            return mdRaw;
        } else {
            let ref = element.ref[0];
            let newRef = 
                `[${ref.id}]: ${require(`./body/contents/${ref.link}`)} \n`;
            let newMD = mdRaw + newRef;
            element.ref.shift();
            return this.contentRef(element, newMD);
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
        const { markdown } = this.state;
       return (
        <div
        className= {`main docs body ${theme}`}
        style={{left, width}}>
            <MarkdownView
                markdown={markdown}
                options={{
                    tables: true
                    , emoji: true
                    , omitExtraWLInCodeBlocks: true
                    , ghCodeBlocks: true
                    , requireSpaceBeforeHeadingText: true
                }}
            />
        </div>
      );
    }
}

export default Body;