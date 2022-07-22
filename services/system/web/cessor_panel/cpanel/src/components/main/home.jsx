import React, { PureComponent } from "react";

class Home extends PureComponent {
    render() {
        let active = this.props.active;
        return active && (
            <div className="col-12">
                <div className={`row`} style={{color:"white"}} >
                    Home
                </div>
            </div>
        );
    }
}

export default Home;