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
-- Module      : Amazonka.SageMaker.Types.ModelPackageGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelPackageGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelPackageGroupStatus
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.UserContext

-- | A group of versioned models in the model registry.
--
-- /See:/ 'newModelPackageGroup' smart constructor.
data ModelPackageGroup = ModelPackageGroup'
  { -- | The time that the model group was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The description for the model group.
    modelPackageGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model group.
    modelPackageGroupArn :: Prelude.Maybe Prelude.Text,
    createdBy :: Prelude.Maybe UserContext,
    -- | The name of the model group.
    modelPackageGroupName :: Prelude.Maybe Prelude.Text,
    -- | The status of the model group. This can be one of the following values.
    --
    -- -   @PENDING@ - The model group is pending being created.
    --
    -- -   @IN_PROGRESS@ - The model group is in the process of being created.
    --
    -- -   @COMPLETED@ - The model group was successfully created.
    --
    -- -   @FAILED@ - The model group failed.
    --
    -- -   @DELETING@ - The model group is in the process of being deleted.
    --
    -- -   @DELETE_FAILED@ - SageMaker failed to delete the model group.
    modelPackageGroupStatus :: Prelude.Maybe ModelPackageGroupStatus,
    -- | A list of the tags associated with the model group. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    -- in the /Amazon Web Services General Reference Guide/.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelPackageGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'modelPackageGroup_creationTime' - The time that the model group was created.
--
-- 'modelPackageGroupDescription', 'modelPackageGroup_modelPackageGroupDescription' - The description for the model group.
--
-- 'modelPackageGroupArn', 'modelPackageGroup_modelPackageGroupArn' - The Amazon Resource Name (ARN) of the model group.
--
-- 'createdBy', 'modelPackageGroup_createdBy' - Undocumented member.
--
-- 'modelPackageGroupName', 'modelPackageGroup_modelPackageGroupName' - The name of the model group.
--
-- 'modelPackageGroupStatus', 'modelPackageGroup_modelPackageGroupStatus' - The status of the model group. This can be one of the following values.
--
-- -   @PENDING@ - The model group is pending being created.
--
-- -   @IN_PROGRESS@ - The model group is in the process of being created.
--
-- -   @COMPLETED@ - The model group was successfully created.
--
-- -   @FAILED@ - The model group failed.
--
-- -   @DELETING@ - The model group is in the process of being deleted.
--
-- -   @DELETE_FAILED@ - SageMaker failed to delete the model group.
--
-- 'tags', 'modelPackageGroup_tags' - A list of the tags associated with the model group. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
newModelPackageGroup ::
  ModelPackageGroup
newModelPackageGroup =
  ModelPackageGroup'
    { creationTime = Prelude.Nothing,
      modelPackageGroupDescription = Prelude.Nothing,
      modelPackageGroupArn = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      modelPackageGroupName = Prelude.Nothing,
      modelPackageGroupStatus = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The time that the model group was created.
modelPackageGroup_creationTime :: Lens.Lens' ModelPackageGroup (Prelude.Maybe Prelude.UTCTime)
modelPackageGroup_creationTime = Lens.lens (\ModelPackageGroup' {creationTime} -> creationTime) (\s@ModelPackageGroup' {} a -> s {creationTime = a} :: ModelPackageGroup) Prelude.. Lens.mapping Core._Time

-- | The description for the model group.
modelPackageGroup_modelPackageGroupDescription :: Lens.Lens' ModelPackageGroup (Prelude.Maybe Prelude.Text)
modelPackageGroup_modelPackageGroupDescription = Lens.lens (\ModelPackageGroup' {modelPackageGroupDescription} -> modelPackageGroupDescription) (\s@ModelPackageGroup' {} a -> s {modelPackageGroupDescription = a} :: ModelPackageGroup)

-- | The Amazon Resource Name (ARN) of the model group.
modelPackageGroup_modelPackageGroupArn :: Lens.Lens' ModelPackageGroup (Prelude.Maybe Prelude.Text)
modelPackageGroup_modelPackageGroupArn = Lens.lens (\ModelPackageGroup' {modelPackageGroupArn} -> modelPackageGroupArn) (\s@ModelPackageGroup' {} a -> s {modelPackageGroupArn = a} :: ModelPackageGroup)

-- | Undocumented member.
modelPackageGroup_createdBy :: Lens.Lens' ModelPackageGroup (Prelude.Maybe UserContext)
modelPackageGroup_createdBy = Lens.lens (\ModelPackageGroup' {createdBy} -> createdBy) (\s@ModelPackageGroup' {} a -> s {createdBy = a} :: ModelPackageGroup)

-- | The name of the model group.
modelPackageGroup_modelPackageGroupName :: Lens.Lens' ModelPackageGroup (Prelude.Maybe Prelude.Text)
modelPackageGroup_modelPackageGroupName = Lens.lens (\ModelPackageGroup' {modelPackageGroupName} -> modelPackageGroupName) (\s@ModelPackageGroup' {} a -> s {modelPackageGroupName = a} :: ModelPackageGroup)

-- | The status of the model group. This can be one of the following values.
--
-- -   @PENDING@ - The model group is pending being created.
--
-- -   @IN_PROGRESS@ - The model group is in the process of being created.
--
-- -   @COMPLETED@ - The model group was successfully created.
--
-- -   @FAILED@ - The model group failed.
--
-- -   @DELETING@ - The model group is in the process of being deleted.
--
-- -   @DELETE_FAILED@ - SageMaker failed to delete the model group.
modelPackageGroup_modelPackageGroupStatus :: Lens.Lens' ModelPackageGroup (Prelude.Maybe ModelPackageGroupStatus)
modelPackageGroup_modelPackageGroupStatus = Lens.lens (\ModelPackageGroup' {modelPackageGroupStatus} -> modelPackageGroupStatus) (\s@ModelPackageGroup' {} a -> s {modelPackageGroupStatus = a} :: ModelPackageGroup)

-- | A list of the tags associated with the model group. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
modelPackageGroup_tags :: Lens.Lens' ModelPackageGroup (Prelude.Maybe [Tag])
modelPackageGroup_tags = Lens.lens (\ModelPackageGroup' {tags} -> tags) (\s@ModelPackageGroup' {} a -> s {tags = a} :: ModelPackageGroup) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ModelPackageGroup where
  parseJSON =
    Core.withObject
      "ModelPackageGroup"
      ( \x ->
          ModelPackageGroup'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "ModelPackageGroupDescription")
            Prelude.<*> (x Core..:? "ModelPackageGroupArn")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "ModelPackageGroupName")
            Prelude.<*> (x Core..:? "ModelPackageGroupStatus")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ModelPackageGroup where
  hashWithSalt salt' ModelPackageGroup' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` modelPackageGroupStatus
      `Prelude.hashWithSalt` modelPackageGroupName
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` modelPackageGroupArn
      `Prelude.hashWithSalt` modelPackageGroupDescription
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData ModelPackageGroup where
  rnf ModelPackageGroup' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf modelPackageGroupStatus
      `Prelude.seq` Prelude.rnf modelPackageGroupName
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf modelPackageGroupArn
      `Prelude.seq` Prelude.rnf modelPackageGroupDescription
