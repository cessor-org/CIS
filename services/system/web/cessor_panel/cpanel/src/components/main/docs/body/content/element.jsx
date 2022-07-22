import React, { PureComponent } from "react";
import ContentElementChild from "./elementChild.jsx";
import {
    faExternalLinkAlt
  } from "@fortawesome/free-solid-svg-icons";
  import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
class ContentElement extends PureComponent {
    constructor(props) {
        super(props);
        this.state = {}
    }
    title = () => {
        let style = this.style({title : true});
        return (
            <div
                className={`title`}
                style = {style}>
                {this.props.element.title}
            </div>
        );
    }
    descript = () => {
        let style = this.style({descript : true});
        let currentIndex = this.props.index;
        let description = 
            (this.props.type == "description")?
                this.props.descript :
                this.props.element.descript;
        return (
            <div
                className={`descript`}
                style = {style}>
                    {description.map((descript, index)=>(
                        (Array.isArray(descript)) ?
                            this.descriptions(descript, index) :
                            (<p
                            key = {`docs-body-conEl-descript-${currentIndex.concat([index])}`}>
                                {descript}
                            </p>)
                    ))}
            </div>
        );
    }
    descriptions = (contents, pIndex) => {
        console.log("1: ", contents);
        let currentIndex = this.props.index;
        return contents.map((content,index)=>(
            (typeof content == 'object') ?
                (<p
                    key = {`docs-body-conEl-desCont-${currentIndex.concat([pIndex, index])}`}
                    className="line link"
                    onClick={()=>{this.props.onChange({link: content.link})}}>
                    {content.title}
                    <FontAwesomeIcon icon={faExternalLinkAlt} className={"icon"}/>
                </p>) :
                (<p
                    key = {`docs-body-conEl-desCont-${currentIndex.concat([pIndex, index])}`}
                    className="line">
                    {content}
                </p>)
            ));
    }
    children = () => {
        let element = this.props.element;
        let currentIndex = this.props.index;
        if (element.children) {
            return element.children.map((child, index)=>(
               <ContentElementChild
                    key = {`docs-body-conEl-${currentIndex.concat([index])}`}
                    index = {currentIndex.concat([index])}
                    element = {child}
                    onChange = {this.fromChild}
               />
            ));
        } else return null;

    }
    fromChild = (e) => {
        this.props.onChange(e);
    }
    style = (type) => {
        let level = this.props.index.length;
        let style = {};
        if (type.title) {
            if (level > 1){
                style.marginTop = 15;
                style.fontSize = 18;
                style.border = "none";
                style.fontWeight = "bold";
            }
        }
        if (type.descript) {
            if (this.props.type == "description") {
                style.paddingLeft = 0;
                style.fontSize = 15;
            }
            else {
                if (level == 1){
                    style.paddingLeft = 5;
                    style.fontSize = 16;
                }
                if (level > 1){
                    style.paddingLeft = 10;
                    style.fontSize = 15;
                }
            }
        }
        return style;
    }
    render() {
        console.log("****** type:" ,this.props.type)
        if (this.props.type == "description")
            return (
                <div
                className={`ContentElement`}
                style = {{}}
                >
                    {this.descript()}
            </div>
            );
        else 
            return (
                <div
                    className={`ContentElement`}
                    style = {{}}
                    >
                        {this.title()}
                        {this.descript()}
                        {this.children()}
                </div>
            );
    }
}

export default ContentElement;