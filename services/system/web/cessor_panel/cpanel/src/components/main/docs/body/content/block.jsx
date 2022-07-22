import React, { PureComponent } from "react";

class Block extends PureComponent {
    constructor(props) {
        super(props);
        this.state = {}
    }
    children = () => {
        return this.props.elements.map((child, index)=>(
            <div
                key = {Math.random()} >
                {this.child(child)}
            </div>
        ));
    }
    child = (element) => {
        if (element.type == "dash") {
            return `- ${element.content}`
        }
    }
    render() {
        return(
            <div className = {`block`}>
                {this.children()}
            </div>
        );
    }
}

export default Block;