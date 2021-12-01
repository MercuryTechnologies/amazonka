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
-- Module      : Amazonka.SageMaker.Types.ModelPackageSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelPackageSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelApprovalStatus
import Amazonka.SageMaker.Types.ModelPackageStatus

-- | Provides summary information about a model package.
--
-- /See:/ 'newModelPackageSummary' smart constructor.
data ModelPackageSummary = ModelPackageSummary'
  { -- | The approval status of the model. This can be one of the following
    -- values.
    --
    -- -   @APPROVED@ - The model is approved
    --
    -- -   @REJECTED@ - The model is rejected.
    --
    -- -   @PENDING_MANUAL_APPROVAL@ - The model is waiting for manual
    --     approval.
    modelApprovalStatus :: Prelude.Maybe ModelApprovalStatus,
    -- | A brief description of the model package.
    modelPackageDescription :: Prelude.Maybe Prelude.Text,
    -- | If the model package is a versioned model, the version of the model.
    modelPackageVersion :: Prelude.Maybe Prelude.Natural,
    -- | If the model package is a versioned model, the model group that the
    -- versioned model belongs to.
    modelPackageGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the model package.
    modelPackageName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model package.
    modelPackageArn :: Prelude.Text,
    -- | A timestamp that shows when the model package was created.
    creationTime :: Core.POSIX,
    -- | The overall status of the model package.
    modelPackageStatus :: ModelPackageStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelPackageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelApprovalStatus', 'modelPackageSummary_modelApprovalStatus' - The approval status of the model. This can be one of the following
-- values.
--
-- -   @APPROVED@ - The model is approved
--
-- -   @REJECTED@ - The model is rejected.
--
-- -   @PENDING_MANUAL_APPROVAL@ - The model is waiting for manual
--     approval.
--
-- 'modelPackageDescription', 'modelPackageSummary_modelPackageDescription' - A brief description of the model package.
--
-- 'modelPackageVersion', 'modelPackageSummary_modelPackageVersion' - If the model package is a versioned model, the version of the model.
--
-- 'modelPackageGroupName', 'modelPackageSummary_modelPackageGroupName' - If the model package is a versioned model, the model group that the
-- versioned model belongs to.
--
-- 'modelPackageName', 'modelPackageSummary_modelPackageName' - The name of the model package.
--
-- 'modelPackageArn', 'modelPackageSummary_modelPackageArn' - The Amazon Resource Name (ARN) of the model package.
--
-- 'creationTime', 'modelPackageSummary_creationTime' - A timestamp that shows when the model package was created.
--
-- 'modelPackageStatus', 'modelPackageSummary_modelPackageStatus' - The overall status of the model package.
newModelPackageSummary ::
  -- | 'modelPackageName'
  Prelude.Text ->
  -- | 'modelPackageArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'modelPackageStatus'
  ModelPackageStatus ->
  ModelPackageSummary
newModelPackageSummary
  pModelPackageName_
  pModelPackageArn_
  pCreationTime_
  pModelPackageStatus_ =
    ModelPackageSummary'
      { modelApprovalStatus =
          Prelude.Nothing,
        modelPackageDescription = Prelude.Nothing,
        modelPackageVersion = Prelude.Nothing,
        modelPackageGroupName = Prelude.Nothing,
        modelPackageName = pModelPackageName_,
        modelPackageArn = pModelPackageArn_,
        creationTime = Core._Time Lens.# pCreationTime_,
        modelPackageStatus = pModelPackageStatus_
      }

-- | The approval status of the model. This can be one of the following
-- values.
--
-- -   @APPROVED@ - The model is approved
--
-- -   @REJECTED@ - The model is rejected.
--
-- -   @PENDING_MANUAL_APPROVAL@ - The model is waiting for manual
--     approval.
modelPackageSummary_modelApprovalStatus :: Lens.Lens' ModelPackageSummary (Prelude.Maybe ModelApprovalStatus)
modelPackageSummary_modelApprovalStatus = Lens.lens (\ModelPackageSummary' {modelApprovalStatus} -> modelApprovalStatus) (\s@ModelPackageSummary' {} a -> s {modelApprovalStatus = a} :: ModelPackageSummary)

-- | A brief description of the model package.
modelPackageSummary_modelPackageDescription :: Lens.Lens' ModelPackageSummary (Prelude.Maybe Prelude.Text)
modelPackageSummary_modelPackageDescription = Lens.lens (\ModelPackageSummary' {modelPackageDescription} -> modelPackageDescription) (\s@ModelPackageSummary' {} a -> s {modelPackageDescription = a} :: ModelPackageSummary)

-- | If the model package is a versioned model, the version of the model.
modelPackageSummary_modelPackageVersion :: Lens.Lens' ModelPackageSummary (Prelude.Maybe Prelude.Natural)
modelPackageSummary_modelPackageVersion = Lens.lens (\ModelPackageSummary' {modelPackageVersion} -> modelPackageVersion) (\s@ModelPackageSummary' {} a -> s {modelPackageVersion = a} :: ModelPackageSummary)

-- | If the model package is a versioned model, the model group that the
-- versioned model belongs to.
modelPackageSummary_modelPackageGroupName :: Lens.Lens' ModelPackageSummary (Prelude.Maybe Prelude.Text)
modelPackageSummary_modelPackageGroupName = Lens.lens (\ModelPackageSummary' {modelPackageGroupName} -> modelPackageGroupName) (\s@ModelPackageSummary' {} a -> s {modelPackageGroupName = a} :: ModelPackageSummary)

-- | The name of the model package.
modelPackageSummary_modelPackageName :: Lens.Lens' ModelPackageSummary Prelude.Text
modelPackageSummary_modelPackageName = Lens.lens (\ModelPackageSummary' {modelPackageName} -> modelPackageName) (\s@ModelPackageSummary' {} a -> s {modelPackageName = a} :: ModelPackageSummary)

-- | The Amazon Resource Name (ARN) of the model package.
modelPackageSummary_modelPackageArn :: Lens.Lens' ModelPackageSummary Prelude.Text
modelPackageSummary_modelPackageArn = Lens.lens (\ModelPackageSummary' {modelPackageArn} -> modelPackageArn) (\s@ModelPackageSummary' {} a -> s {modelPackageArn = a} :: ModelPackageSummary)

-- | A timestamp that shows when the model package was created.
modelPackageSummary_creationTime :: Lens.Lens' ModelPackageSummary Prelude.UTCTime
modelPackageSummary_creationTime = Lens.lens (\ModelPackageSummary' {creationTime} -> creationTime) (\s@ModelPackageSummary' {} a -> s {creationTime = a} :: ModelPackageSummary) Prelude.. Core._Time

-- | The overall status of the model package.
modelPackageSummary_modelPackageStatus :: Lens.Lens' ModelPackageSummary ModelPackageStatus
modelPackageSummary_modelPackageStatus = Lens.lens (\ModelPackageSummary' {modelPackageStatus} -> modelPackageStatus) (\s@ModelPackageSummary' {} a -> s {modelPackageStatus = a} :: ModelPackageSummary)

instance Core.FromJSON ModelPackageSummary where
  parseJSON =
    Core.withObject
      "ModelPackageSummary"
      ( \x ->
          ModelPackageSummary'
            Prelude.<$> (x Core..:? "ModelApprovalStatus")
            Prelude.<*> (x Core..:? "ModelPackageDescription")
            Prelude.<*> (x Core..:? "ModelPackageVersion")
            Prelude.<*> (x Core..:? "ModelPackageGroupName")
            Prelude.<*> (x Core..: "ModelPackageName")
            Prelude.<*> (x Core..: "ModelPackageArn")
            Prelude.<*> (x Core..: "CreationTime")
            Prelude.<*> (x Core..: "ModelPackageStatus")
      )

instance Prelude.Hashable ModelPackageSummary where
  hashWithSalt salt' ModelPackageSummary' {..} =
    salt' `Prelude.hashWithSalt` modelPackageStatus
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` modelPackageArn
      `Prelude.hashWithSalt` modelPackageName
      `Prelude.hashWithSalt` modelPackageGroupName
      `Prelude.hashWithSalt` modelPackageVersion
      `Prelude.hashWithSalt` modelPackageDescription
      `Prelude.hashWithSalt` modelApprovalStatus

instance Prelude.NFData ModelPackageSummary where
  rnf ModelPackageSummary' {..} =
    Prelude.rnf modelApprovalStatus
      `Prelude.seq` Prelude.rnf modelPackageStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf modelPackageArn
      `Prelude.seq` Prelude.rnf modelPackageName
      `Prelude.seq` Prelude.rnf modelPackageGroupName
      `Prelude.seq` Prelude.rnf modelPackageVersion
      `Prelude.seq` Prelude.rnf modelPackageDescription
