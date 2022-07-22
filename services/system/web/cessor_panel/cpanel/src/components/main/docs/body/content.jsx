import React, { PureComponent } from "react";
import ContentElement from "./content/element.jsx"
import {
    faExternalLinkAlt
  } from "@fortawesome/free-solid-svg-icons";
  import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
class Content extends PureComponent {
    constructor(props) {
        super(props);
        this.state = {}
    }
    componentDidMount() {}
    title =() => {
        let content = this.props.content;
        if (content && content.title) {
            return (
                <div
                    className = {`title-1`}>
                        {this.props.content.title}
                </div>
            );
        }
        else return null
    }
    descript = () => {
        let content = this.props.content;
        if (content && content.descript) {
            return (
                <ContentElement
                    key = {`main-body-description`}
                    type = "description"
                    index = {[this.props.index]}
                    descript = {content.descript}
                    onChange = {this.fromChild}
                />
            );
        }
        else return null
    }
    descript0 = () => {
        let content = this.props.content;
        let style = {fontSize : 16};
        if (content && content.descript) {
            return (
                <div
                    className={`descript`}
                    style = {style}>
                        {content.descript.map((child, index)=>(
                            <p
                                key={Math.random()}>
                                {child}
                            </p>
                        ))}
                </div>
            );
        }
        else return null
    }
    children =()=> {
        let content = this.props.content;
        if (content && content.children) {
            return content.children.map((element, index)=>(
                <ContentElement
                    key = {`main-body-element-${index}`}
                    type = "element"
                    index = {[index]}
                    element = {element}
                    onChange = {this.fromChild}
                />
            ));
        }
        else return null
    }
    fromChild = (e) => {
        this.props.onChange(e);
    }
    render() {
        
        return (
            <div
                className= {``}
                style={{}}>
                    {this.title()}
                    {this.descript()}
                    {this.children()}
            </div>
        );
    }
}

export default Content;