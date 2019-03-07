import * as React from 'react';
import * as QrReader from 'react-qr-reader';

export class MyScanner extends React.Component {
  public state = {
    result: 'No result',
  }
 
  public handleScan = (data: any) => {
    // console.log(data);
    if (data) {
      console.log(data);
      this.setState({
        result: data,
      })
    }
  }
  public handleError = (err: any) => {
    console.error(err);
  }
  public render() {
    return (
      <div>
        <QrReader
          delay={ 300 }
          onError={this.handleError}
          onScan={this.handleScan}
          style={{ width: '30%' }}
        />
        <p> {this.state.result} </p>
      </div>
    )
  }
}
