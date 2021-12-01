{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Personalize.Types.SolutionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.SolutionVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Personalize.Types.SolutionConfig
import Amazonka.Personalize.Types.TrainingMode
import Amazonka.Personalize.Types.TunedHPOParams
import qualified Amazonka.Prelude as Prelude

-- | An object that provides information about a specific version of a
-- Solution.
--
-- /See:/ 'newSolutionVersion' smart constructor.
data SolutionVersion = SolutionVersion'
  { -- | If training a solution version fails, the reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the solution.
    solutionArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the solution version.
    --
    -- A solution version can be in one of the following states:
    --
    -- -   CREATE PENDING
    --
    -- -   CREATE IN_PROGRESS
    --
    -- -   ACTIVE
    --
    -- -   CREATE FAILED
    --
    -- -   CREATE STOPPING
    --
    -- -   CREATE STOPPED
    status :: Prelude.Maybe Prelude.Text,
    -- | When true, Amazon Personalize searches for the most optimal recipe
    -- according to the solution configuration. When false (the default),
    -- Amazon Personalize uses @recipeArn@.
    performAutoML :: Prelude.Maybe Prelude.Bool,
    -- | If hyperparameter optimization was performed, contains the
    -- hyperparameter values of the best performing model.
    tunedHPOParams :: Prelude.Maybe TunedHPOParams,
    -- | The ARN of the recipe used in the solution.
    recipeArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the solution was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The event type (for example, \'click\' or \'like\') that is used for
    -- training the model.
    eventType :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that this version of the solution was
    -- created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset group providing the
    -- training data.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The scope of training to be performed when creating the solution
    -- version. The @FULL@ option trains the solution version based on the
    -- entirety of the input solution\'s training data, while the @UPDATE@
    -- option processes only the data that has changed in comparison to the
    -- input solution. Choose @UPDATE@ when you want to incrementally update
    -- your solution version instead of creating an entirely new one.
    --
    -- The @UPDATE@ option can only be used when you already have an active
    -- solution version created from the input solution using the @FULL@ option
    -- and the input solution was trained with the
    -- <https://docs.aws.amazon.com/personalize/latest/dg/native-recipe-new-item-USER_PERSONALIZATION.html User-Personalization>
    -- recipe or the
    -- <https://docs.aws.amazon.com/personalize/latest/dg/native-recipe-hrnn-coldstart.html HRNN-Coldstart>
    -- recipe.
    trainingMode :: Prelude.Maybe TrainingMode,
    -- | The time used to train the model. You are billed for the time it takes
    -- to train a model. This field is visible only after Amazon Personalize
    -- successfully trains a model.
    trainingHours :: Prelude.Maybe Prelude.Double,
    -- | Describes the configuration properties for the solution.
    solutionConfig :: Prelude.Maybe SolutionConfig,
    -- | Whether to perform hyperparameter optimization (HPO) on the chosen
    -- recipe. The default is @false@.
    performHPO :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the solution version.
    solutionVersionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SolutionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'solutionVersion_failureReason' - If training a solution version fails, the reason for the failure.
--
-- 'solutionArn', 'solutionVersion_solutionArn' - The ARN of the solution.
--
-- 'status', 'solutionVersion_status' - The status of the solution version.
--
-- A solution version can be in one of the following states:
--
-- -   CREATE PENDING
--
-- -   CREATE IN_PROGRESS
--
-- -   ACTIVE
--
-- -   CREATE FAILED
--
-- -   CREATE STOPPING
--
-- -   CREATE STOPPED
--
-- 'performAutoML', 'solutionVersion_performAutoML' - When true, Amazon Personalize searches for the most optimal recipe
-- according to the solution configuration. When false (the default),
-- Amazon Personalize uses @recipeArn@.
--
-- 'tunedHPOParams', 'solutionVersion_tunedHPOParams' - If hyperparameter optimization was performed, contains the
-- hyperparameter values of the best performing model.
--
-- 'recipeArn', 'solutionVersion_recipeArn' - The ARN of the recipe used in the solution.
--
-- 'lastUpdatedDateTime', 'solutionVersion_lastUpdatedDateTime' - The date and time (in Unix time) that the solution was last updated.
--
-- 'eventType', 'solutionVersion_eventType' - The event type (for example, \'click\' or \'like\') that is used for
-- training the model.
--
-- 'creationDateTime', 'solutionVersion_creationDateTime' - The date and time (in Unix time) that this version of the solution was
-- created.
--
-- 'datasetGroupArn', 'solutionVersion_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group providing the
-- training data.
--
-- 'trainingMode', 'solutionVersion_trainingMode' - The scope of training to be performed when creating the solution
-- version. The @FULL@ option trains the solution version based on the
-- entirety of the input solution\'s training data, while the @UPDATE@
-- option processes only the data that has changed in comparison to the
-- input solution. Choose @UPDATE@ when you want to incrementally update
-- your solution version instead of creating an entirely new one.
--
-- The @UPDATE@ option can only be used when you already have an active
-- solution version created from the input solution using the @FULL@ option
-- and the input solution was trained with the
-- <https://docs.aws.amazon.com/personalize/latest/dg/native-recipe-new-item-USER_PERSONALIZATION.html User-Personalization>
-- recipe or the
-- <https://docs.aws.amazon.com/personalize/latest/dg/native-recipe-hrnn-coldstart.html HRNN-Coldstart>
-- recipe.
--
-- 'trainingHours', 'solutionVersion_trainingHours' - The time used to train the model. You are billed for the time it takes
-- to train a model. This field is visible only after Amazon Personalize
-- successfully trains a model.
--
-- 'solutionConfig', 'solutionVersion_solutionConfig' - Describes the configuration properties for the solution.
--
-- 'performHPO', 'solutionVersion_performHPO' - Whether to perform hyperparameter optimization (HPO) on the chosen
-- recipe. The default is @false@.
--
-- 'solutionVersionArn', 'solutionVersion_solutionVersionArn' - The ARN of the solution version.
newSolutionVersion ::
  SolutionVersion
newSolutionVersion =
  SolutionVersion'
    { failureReason = Prelude.Nothing,
      solutionArn = Prelude.Nothing,
      status = Prelude.Nothing,
      performAutoML = Prelude.Nothing,
      tunedHPOParams = Prelude.Nothing,
      recipeArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      eventType = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      trainingMode = Prelude.Nothing,
      trainingHours = Prelude.Nothing,
      solutionConfig = Prelude.Nothing,
      performHPO = Prelude.Nothing,
      solutionVersionArn = Prelude.Nothing
    }

-- | If training a solution version fails, the reason for the failure.
solutionVersion_failureReason :: Lens.Lens' SolutionVersion (Prelude.Maybe Prelude.Text)
solutionVersion_failureReason = Lens.lens (\SolutionVersion' {failureReason} -> failureReason) (\s@SolutionVersion' {} a -> s {failureReason = a} :: SolutionVersion)

-- | The ARN of the solution.
solutionVersion_solutionArn :: Lens.Lens' SolutionVersion (Prelude.Maybe Prelude.Text)
solutionVersion_solutionArn = Lens.lens (\SolutionVersion' {solutionArn} -> solutionArn) (\s@SolutionVersion' {} a -> s {solutionArn = a} :: SolutionVersion)

-- | The status of the solution version.
--
-- A solution version can be in one of the following states:
--
-- -   CREATE PENDING
--
-- -   CREATE IN_PROGRESS
--
-- -   ACTIVE
--
-- -   CREATE FAILED
--
-- -   CREATE STOPPING
--
-- -   CREATE STOPPED
solutionVersion_status :: Lens.Lens' SolutionVersion (Prelude.Maybe Prelude.Text)
solutionVersion_status = Lens.lens (\SolutionVersion' {status} -> status) (\s@SolutionVersion' {} a -> s {status = a} :: SolutionVersion)

-- | When true, Amazon Personalize searches for the most optimal recipe
-- according to the solution configuration. When false (the default),
-- Amazon Personalize uses @recipeArn@.
solutionVersion_performAutoML :: Lens.Lens' SolutionVersion (Prelude.Maybe Prelude.Bool)
solutionVersion_performAutoML = Lens.lens (\SolutionVersion' {performAutoML} -> performAutoML) (\s@SolutionVersion' {} a -> s {performAutoML = a} :: SolutionVersion)

-- | If hyperparameter optimization was performed, contains the
-- hyperparameter values of the best performing model.
solutionVersion_tunedHPOParams :: Lens.Lens' SolutionVersion (Prelude.Maybe TunedHPOParams)
solutionVersion_tunedHPOParams = Lens.lens (\SolutionVersion' {tunedHPOParams} -> tunedHPOParams) (\s@SolutionVersion' {} a -> s {tunedHPOParams = a} :: SolutionVersion)

-- | The ARN of the recipe used in the solution.
solutionVersion_recipeArn :: Lens.Lens' SolutionVersion (Prelude.Maybe Prelude.Text)
solutionVersion_recipeArn = Lens.lens (\SolutionVersion' {recipeArn} -> recipeArn) (\s@SolutionVersion' {} a -> s {recipeArn = a} :: SolutionVersion)

-- | The date and time (in Unix time) that the solution was last updated.
solutionVersion_lastUpdatedDateTime :: Lens.Lens' SolutionVersion (Prelude.Maybe Prelude.UTCTime)
solutionVersion_lastUpdatedDateTime = Lens.lens (\SolutionVersion' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@SolutionVersion' {} a -> s {lastUpdatedDateTime = a} :: SolutionVersion) Prelude.. Lens.mapping Core._Time

-- | The event type (for example, \'click\' or \'like\') that is used for
-- training the model.
solutionVersion_eventType :: Lens.Lens' SolutionVersion (Prelude.Maybe Prelude.Text)
solutionVersion_eventType = Lens.lens (\SolutionVersion' {eventType} -> eventType) (\s@SolutionVersion' {} a -> s {eventType = a} :: SolutionVersion)

-- | The date and time (in Unix time) that this version of the solution was
-- created.
solutionVersion_creationDateTime :: Lens.Lens' SolutionVersion (Prelude.Maybe Prelude.UTCTime)
solutionVersion_creationDateTime = Lens.lens (\SolutionVersion' {creationDateTime} -> creationDateTime) (\s@SolutionVersion' {} a -> s {creationDateTime = a} :: SolutionVersion) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the dataset group providing the
-- training data.
solutionVersion_datasetGroupArn :: Lens.Lens' SolutionVersion (Prelude.Maybe Prelude.Text)
solutionVersion_datasetGroupArn = Lens.lens (\SolutionVersion' {datasetGroupArn} -> datasetGroupArn) (\s@SolutionVersion' {} a -> s {datasetGroupArn = a} :: SolutionVersion)

-- | The scope of training to be performed when creating the solution
-- version. The @FULL@ option trains the solution version based on the
-- entirety of the input solution\'s training data, while the @UPDATE@
-- option processes only the data that has changed in comparison to the
-- input solution. Choose @UPDATE@ when you want to incrementally update
-- your solution version instead of creating an entirely new one.
--
-- The @UPDATE@ option can only be used when you already have an active
-- solution version created from the input solution using the @FULL@ option
-- and the input solution was trained with the
-- <https://docs.aws.amazon.com/personalize/latest/dg/native-recipe-new-item-USER_PERSONALIZATION.html User-Personalization>
-- recipe or the
-- <https://docs.aws.amazon.com/personalize/latest/dg/native-recipe-hrnn-coldstart.html HRNN-Coldstart>
-- recipe.
solutionVersion_trainingMode :: Lens.Lens' SolutionVersion (Prelude.Maybe TrainingMode)
solutionVersion_trainingMode = Lens.lens (\SolutionVersion' {trainingMode} -> trainingMode) (\s@SolutionVersion' {} a -> s {trainingMode = a} :: SolutionVersion)

-- | The time used to train the model. You are billed for the time it takes
-- to train a model. This field is visible only after Amazon Personalize
-- successfully trains a model.
solutionVersion_trainingHours :: Lens.Lens' SolutionVersion (Prelude.Maybe Prelude.Double)
solutionVersion_trainingHours = Lens.lens (\SolutionVersion' {trainingHours} -> trainingHours) (\s@SolutionVersion' {} a -> s {trainingHours = a} :: SolutionVersion)

-- | Describes the configuration properties for the solution.
solutionVersion_solutionConfig :: Lens.Lens' SolutionVersion (Prelude.Maybe SolutionConfig)
solutionVersion_solutionConfig = Lens.lens (\SolutionVersion' {solutionConfig} -> solutionConfig) (\s@SolutionVersion' {} a -> s {solutionConfig = a} :: SolutionVersion)

-- | Whether to perform hyperparameter optimization (HPO) on the chosen
-- recipe. The default is @false@.
solutionVersion_performHPO :: Lens.Lens' SolutionVersion (Prelude.Maybe Prelude.Bool)
solutionVersion_performHPO = Lens.lens (\SolutionVersion' {performHPO} -> performHPO) (\s@SolutionVersion' {} a -> s {performHPO = a} :: SolutionVersion)

-- | The ARN of the solution version.
solutionVersion_solutionVersionArn :: Lens.Lens' SolutionVersion (Prelude.Maybe Prelude.Text)
solutionVersion_solutionVersionArn = Lens.lens (\SolutionVersion' {solutionVersionArn} -> solutionVersionArn) (\s@SolutionVersion' {} a -> s {solutionVersionArn = a} :: SolutionVersion)

instance Core.FromJSON SolutionVersion where
  parseJSON =
    Core.withObject
      "SolutionVersion"
      ( \x ->
          SolutionVersion'
            Prelude.<$> (x Core..:? "failureReason")
            Prelude.<*> (x Core..:? "solutionArn")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "performAutoML")
            Prelude.<*> (x Core..:? "tunedHPOParams")
            Prelude.<*> (x Core..:? "recipeArn")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "eventType")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "datasetGroupArn")
            Prelude.<*> (x Core..:? "trainingMode")
            Prelude.<*> (x Core..:? "trainingHours")
            Prelude.<*> (x Core..:? "solutionConfig")
            Prelude.<*> (x Core..:? "performHPO")
            Prelude.<*> (x Core..:? "solutionVersionArn")
      )

instance Prelude.Hashable SolutionVersion where
  hashWithSalt salt' SolutionVersion' {..} =
    salt' `Prelude.hashWithSalt` solutionVersionArn
      `Prelude.hashWithSalt` performHPO
      `Prelude.hashWithSalt` solutionConfig
      `Prelude.hashWithSalt` trainingHours
      `Prelude.hashWithSalt` trainingMode
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` recipeArn
      `Prelude.hashWithSalt` tunedHPOParams
      `Prelude.hashWithSalt` performAutoML
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` solutionArn
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData SolutionVersion where
  rnf SolutionVersion' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf solutionVersionArn
      `Prelude.seq` Prelude.rnf performHPO
      `Prelude.seq` Prelude.rnf solutionConfig
      `Prelude.seq` Prelude.rnf trainingHours
      `Prelude.seq` Prelude.rnf trainingMode
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf recipeArn
      `Prelude.seq` Prelude.rnf tunedHPOParams
      `Prelude.seq` Prelude.rnf performAutoML
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf solutionArn
