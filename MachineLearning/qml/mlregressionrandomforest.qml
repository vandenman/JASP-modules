//
// Copyright (C) 2013-2018 University of Amsterdam
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
            singleItem: true
            allowedColumns: ["scale"]
        }
        AssignedVariablesList {
                    name: "predictors"
                    title: qsTr("Predictors")
                    singleItem: false
                    allowedColumns: ["nominal", "scale", "ordinal"]
                }
        AssignedVariablesList {
                    name: "indicator"
                    title: qsTr("Apply indicator (optional)")
                    singleItem: true
                    allowedColumns: ["nominal"]
                    id: indicator
                }
    }

    CheckBox { name: "tableVariableImportance";	text: qsTr("Variable importance table")                                  }
    CheckBox { name: "applyImpute";	text: qsTr("Apply model to missing target values"); enabled: indicator.checked      }

    ExpanderButton
    {
        title: qsTr("Model Specifications")

        GridLayout
        {
            RadioButtonGroup
            {
                name: "noOfTrees"
                RadioButton { value: "auto";	text: qsTr("Auto"); checked: true		}
                RadioButton { value: "manual";	text: qsTr("Manual"); id: noOfTrees 	}
                IntegerField
                {
                    name: "numberOfTrees"
                    text: qsTr("No. of trees:")
                    defaultValue: 500
                    fieldWidth: 50
                    enabled: noOfTrees.checked
                    indent: true
                }
            }

            RadioButtonGroup
            {
                name: "noOfPredictors"
                RadioButton { value: "auto";	text: qsTr("Auto"); checked: true           }
                RadioButton { value: "manual";	text: qsTr("Manual"); id: noOfPredictors	}
                IntegerField
                {
                    name: "numberOfPredictors"
                    text: qsTr("No. of predictors:")
                    defaultValue: 1
                    fieldWidth: 50
                    enabled: noOfPredictors.checked
                    indent: true
                }
            }

            RadioButtonGroup
            {
                name: "dataTrainingModel"
                RadioButton { value: "auto";	text: qsTr("Auto"); checked: true           }
                RadioButton { value: "manual";	text: qsTr("Manual"); id: dataTrainingModel	}
                DoubleField
                {
                    name: "percentageDataTraining"
                    doubleValidator { bottom: 0; top: 1; decimals: 2 }
                    text: qsTr("% of data used for training:")
                    defaultValue: 0.8
                    enabled: dataTrainingModel.checked
                    indent: true
                }
            }

            RadioButtonGroup
            {
                name: "dataBootstrapModel"
                RadioButton { value: "auto";	text: qsTr("Auto"); checked: true           }
                RadioButton { value: "manual";	text: qsTr("Manual"); id: dataBootstrapModel	}
                DoubleField
                {
                    name: "percentageDataBootstrap"
                    doubleValidator { bottom: 0; top: 1; decimals: 2 }
                    text: qsTr("% of training data bootstrapped:")
                    defaultValue: 0.63
                    enabled: dataBootstrapModel.checked
                    indent: true
                }
            }
        }
    }

    ExpanderButton
    {
        title: qsTr("Plots")

        Group
        {
            CheckBox { name: "plotVarImp";              text: qsTr("Variable importance plots")                    			}
            CheckBox { name: "plotTreesVsModelError";   text: qsTr("Trees vs. model error plot")        					}
            CheckBox { name: "plotPredPerformance";     text: qsTr("Predictive performance plot")							}
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
                RadioButton { value: "auto";	text: qsTr("Auto"); checked: true           }
                RadioButton { value: "manual";	text: qsTr("Manual"); id: seedBox           }
                DoubleField
                {
                    name: "seed"
                    text: qsTr("Seed:")
                    doubleValidator { bottom: -99999999; top: 99999999; decimals: 2 }
                    defaultValue: 1
                    fieldWidth: 50
                    enabled: seedBox.checked
                    indent: true
                }
            }
            }

            GroupBox
            {

            title: qsTr("Missing values")

            RadioButtonGroup
            {
                name: "missingValues"
                RadioButton { value: "na.omit";	text: qsTr("Omit NA rows"); checked: true                 }
                RadioButton { value: "roughfix";	text: qsTr("Apply roughfix to NAs"); id: naAction          }
            }
            }

        }
    }
}
