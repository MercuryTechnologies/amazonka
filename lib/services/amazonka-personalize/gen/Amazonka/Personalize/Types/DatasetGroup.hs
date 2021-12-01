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
-- Module      : Amazonka.Personalize.Types.DatasetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A dataset group is a collection of related datasets (Interactions, User,
-- and Item). You create a dataset group by calling CreateDatasetGroup. You
-- then create a dataset and add it to a dataset group by calling
-- CreateDataset. The dataset group is used to create and train a solution
-- by calling CreateSolution. A dataset group can contain only one of each
-- type of dataset.
--
-- You can specify an Key Management Service (KMS) key to encrypt the
-- datasets in the group.
--
-- /See:/ 'newDatasetGroup' smart constructor.
data DatasetGroup = DatasetGroup'
  { -- | If creating a dataset group fails, provides the reason why.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The current status of the dataset group.
    --
    -- A dataset group can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    --
    -- -   DELETE PENDING
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Key Management Service (KMS) key
    -- used to encrypt the datasets.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The last update date and time (in Unix time) of the dataset group.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the dataset group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The creation date and time (in Unix time) of the dataset group.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset group.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that has permissions to create the dataset
    -- group.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'datasetGroup_failureReason' - If creating a dataset group fails, provides the reason why.
--
-- 'status', 'datasetGroup_status' - The current status of the dataset group.
--
-- A dataset group can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING
--
-- 'kmsKeyArn', 'datasetGroup_kmsKeyArn' - The Amazon Resource Name (ARN) of the Key Management Service (KMS) key
-- used to encrypt the datasets.
--
-- 'lastUpdatedDateTime', 'datasetGroup_lastUpdatedDateTime' - The last update date and time (in Unix time) of the dataset group.
--
-- 'name', 'datasetGroup_name' - The name of the dataset group.
--
-- 'creationDateTime', 'datasetGroup_creationDateTime' - The creation date and time (in Unix time) of the dataset group.
--
-- 'datasetGroupArn', 'datasetGroup_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group.
--
-- 'roleArn', 'datasetGroup_roleArn' - The ARN of the IAM role that has permissions to create the dataset
-- group.
newDatasetGroup ::
  DatasetGroup
newDatasetGroup =
  DatasetGroup'
    { failureReason = Prelude.Nothing,
      status = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | If creating a dataset group fails, provides the reason why.
datasetGroup_failureReason :: Lens.Lens' DatasetGroup (Prelude.Maybe Prelude.Text)
datasetGroup_failureReason = Lens.lens (\DatasetGroup' {failureReason} -> failureReason) (\s@DatasetGroup' {} a -> s {failureReason = a} :: DatasetGroup)

-- | The current status of the dataset group.
--
-- A dataset group can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING
datasetGroup_status :: Lens.Lens' DatasetGroup (Prelude.Maybe Prelude.Text)
datasetGroup_status = Lens.lens (\DatasetGroup' {status} -> status) (\s@DatasetGroup' {} a -> s {status = a} :: DatasetGroup)

-- | The Amazon Resource Name (ARN) of the Key Management Service (KMS) key
-- used to encrypt the datasets.
datasetGroup_kmsKeyArn :: Lens.Lens' DatasetGroup (Prelude.Maybe Prelude.Text)
datasetGroup_kmsKeyArn = Lens.lens (\DatasetGroup' {kmsKeyArn} -> kmsKeyArn) (\s@DatasetGroup' {} a -> s {kmsKeyArn = a} :: DatasetGroup)

-- | The last update date and time (in Unix time) of the dataset group.
datasetGroup_lastUpdatedDateTime :: Lens.Lens' DatasetGroup (Prelude.Maybe Prelude.UTCTime)
datasetGroup_lastUpdatedDateTime = Lens.lens (\DatasetGroup' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetGroup' {} a -> s {lastUpdatedDateTime = a} :: DatasetGroup) Prelude.. Lens.mapping Core._Time

-- | The name of the dataset group.
datasetGroup_name :: Lens.Lens' DatasetGroup (Prelude.Maybe Prelude.Text)
datasetGroup_name = Lens.lens (\DatasetGroup' {name} -> name) (\s@DatasetGroup' {} a -> s {name = a} :: DatasetGroup)

-- | The creation date and time (in Unix time) of the dataset group.
datasetGroup_creationDateTime :: Lens.Lens' DatasetGroup (Prelude.Maybe Prelude.UTCTime)
datasetGroup_creationDateTime = Lens.lens (\DatasetGroup' {creationDateTime} -> creationDateTime) (\s@DatasetGroup' {} a -> s {creationDateTime = a} :: DatasetGroup) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the dataset group.
datasetGroup_datasetGroupArn :: Lens.Lens' DatasetGroup (Prelude.Maybe Prelude.Text)
datasetGroup_datasetGroupArn = Lens.lens (\DatasetGroup' {datasetGroupArn} -> datasetGroupArn) (\s@DatasetGroup' {} a -> s {datasetGroupArn = a} :: DatasetGroup)

-- | The ARN of the IAM role that has permissions to create the dataset
-- group.
datasetGroup_roleArn :: Lens.Lens' DatasetGroup (Prelude.Maybe Prelude.Text)
datasetGroup_roleArn = Lens.lens (\DatasetGroup' {roleArn} -> roleArn) (\s@DatasetGroup' {} a -> s {roleArn = a} :: DatasetGroup)

instance Core.FromJSON DatasetGroup where
  parseJSON =
    Core.withObject
      "DatasetGroup"
      ( \x ->
          DatasetGroup'
            Prelude.<$> (x Core..:? "failureReason")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "kmsKeyArn")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "datasetGroupArn")
            Prelude.<*> (x Core..:? "roleArn")
      )

instance Prelude.Hashable DatasetGroup where
  hashWithSalt salt' DatasetGroup' {..} =
    salt' `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData DatasetGroup where
  rnf DatasetGroup' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf status
