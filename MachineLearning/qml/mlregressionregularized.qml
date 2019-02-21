//
// Copyright (C) 2013-2019 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Theme 1.0

// All Analysis forms must be built with the From QML item
Form
{
    usesJaspResults: true

    VariablesForm {
        AssignedVariablesList {
            name: "target"
            title: qsTr("Target")
            singleVariable: true
            allowedColumns: ["scale"]
        }
        AssignedVariablesList {
                    name: "predictors"
                    title: qsTr("Predictors")
                    singleVariable: false
                    allowedColumns: ["nominal", "scale", "ordinal"]
                }
        AssignedVariablesList {
                    name: "indicator"
                    title: qsTr("Apply indicator (optional)")
                    singleVariable: true
                    allowedColumns: ["nominal"]
                }
    }

    GroupBox
    {

    title: qsTr("Penalty")

    RadioButtonGroup
    {
        name: "penalty"
        RadioButton { value: "ridge"      ; text: qsTr("Ridge"); checked: true        }
        RadioButton { value: "lasso"      ; text: qsTr("Lasso")                       }
        RadioButton { value: "elasticNet" ; text: qsTr("Elastic Net")                 }
    }
    }

    GroupBox
    {

    title: qsTr("Model application")

    RadioButtonGroup
    {
        name: "applyModel"
        RadioButton { value: "noApp"         ; text: qsTr("Do not apply model"); checked: true        }
        RadioButton { value: "applyIndicator"; text: qsTr("Apply model according to indicator")       }
        RadioButton { value: "applyImpute"   ; text: qsTr("Apply model to missing values in target")  }
    }
    }

    GroupBox {
        title: qsTr("Tables")

        CheckBox { name: "classBoostRelInfTable";	text: qsTr("Relative Influence Table")            }
    }

    ExpanderButton
    {
        title: qsTr("Model Specifications")

        GridLayout
        {

            RadioButtonGroup {
                title: qsTr("Shrinkage")
                name: "shrinkage"
                RadioButton { text: qsTr("Auto")    ; name: "auto"   ; checked: true              }
                RadioButton { text: qsTr("Optimize"); name: "opt"                                 }
                RadioButton { text: qsTr("Manual")  ; name: "manual" ; childrenOnSameRow: true
                    DoubleField { name: "lambda"   ; defaultValue: 0.2; min: 0; max: 1            }
                }
            }

            RadioButtonGroup {
                title: qsTr("Alpha (elastic net only)")
                name: "elasticSpec"
                RadioButton { text: qsTr("Auto")    ; name: "auto"  ; checked: true               }
                RadioButton { text: qsTr("Optimize"); name: "opt"                                 }
                RadioButton { text: qsTr("Manual")  ; name: "manual"; childrenOnSameRow: true
                    DoubleField { name: "alphaElastic"   ; defaultValue: 0.5; min: 0; max: 1             }
                }
            }

            RadioButtonGroup
            {
                title: qsTr("Data used for training")
                name: "dataTrain"
                RadioButton { value: "auto";	text: qsTr("Auto"); checked: true           }
                RadioButton { value: "manual";	text: qsTr("Manual"); childrenOnSameRow: true
                    PercentField { name: "percentageDataTraining"; defaultValue: 80 }
                }
            }

        }
    }

    ExpanderButton
    {
        title: qsTr("Plots")

        Group
        {
            CheckBox { name: "plotPredPerf";       text: qsTr("Predictive performance plot")   }
        }
    }

    ExpanderButton
    {
        title: qsTr("Advanced")

        GridLayout{

            GroupBox
            {

            title: qsTr("Set seed")

            RadioButtonGroup
            {
                name: "seedBox"
                RadioButton { value: "auto"  ;	text: qsTr("Auto")  ; checked: true           }
                RadioButton { value: "manual";	text: qsTr("Manual"); childrenOnSameRow: true
                    DoubleField { name: "seed"; defaultValue: 1 }
                }
            }
            }

            GroupBox
            {

            title: qsTr("Missing values")

            RadioButtonGroup
            {
                name: "NAs"
                RadioButton { value: "na.omit" ; text: qsTr("Omit all rows that contain NAs"); checked: true  }
                RadioButton { value: "roughfix"; text: qsTr("Apply roughfix to NAs")                          }
            }
            }

        }
    }
}
