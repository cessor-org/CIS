import React, { PureComponent } from "react";
import ContentElement from "./element.jsx";
import Block from "./block.jsx";
import cloneDeep from "lodash/cloneDeep";

class ContentElementChild extends PureComponent {
    constructor(props) {
        super(props);
        this.state = {}
    }
    type = () => {
        if (this.props.element.type == "block")
            return this.block();
        else if (this.props.element.type == "dot")
            return this.dot();
        else if (this.props.element.type == "sub")
            return this.sub();
        else return null;
    }
    block = () => {
        if (this.props.element.children) {
            return (
                <Block
                    elements = {this.props.element.children}
                />
            );
        }
    }
    dot = () => {
        return(
            <div
                className = {`dot`}>
                {`* ${this.props.element.content}`}
            </div>
        );
    }
    sub = () => {
        let currentIndex = this.props.index;
        return(
            <ContentElement
                index = {currentIndex.concat([0])}
                type = "sub"
                element = {this.props.element}
                onChange = {this.fromChild}
            />
        );
    }
    fromChild = (e) => {
        this.props.onChange(e);
    }
    render() {
        return (
            <div
                className= {`children`}>
                    {this.type()}
            </div>
        );
    }
}

export default ContentElementChild;