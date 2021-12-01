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
-- Module      : Amazonka.KinesisAnalytics.Types.ReferenceDataSourceUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.ReferenceDataSourceUpdate where

import qualified Amazonka.Core as Core
import Amazonka.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
import Amazonka.KinesisAnalytics.Types.SourceSchema
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | When you update a reference data source configuration for an
-- application, this object provides all the updated values (such as the
-- source bucket name and object key name), the in-application table name
-- that is created, and updated mapping information that maps the data in
-- the Amazon S3 object to the in-application reference table that is
-- created.
--
-- /See:/ 'newReferenceDataSourceUpdate' smart constructor.
data ReferenceDataSourceUpdate = ReferenceDataSourceUpdate'
  { -- | In-application table name that is created by this update.
    tableNameUpdate :: Prelude.Maybe Prelude.Text,
    -- | Describes the S3 bucket name, object key name, and IAM role that Amazon
    -- Kinesis Analytics can assume to read the Amazon S3 object on your behalf
    -- and populate the in-application reference table.
    s3ReferenceDataSourceUpdate :: Prelude.Maybe S3ReferenceDataSourceUpdate,
    -- | Describes the format of the data in the streaming source, and how each
    -- data element maps to corresponding columns created in the in-application
    -- stream.
    referenceSchemaUpdate :: Prelude.Maybe SourceSchema,
    -- | ID of the reference data source being updated. You can use the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
    -- operation to get this value.
    referenceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceDataSourceUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableNameUpdate', 'referenceDataSourceUpdate_tableNameUpdate' - In-application table name that is created by this update.
--
-- 's3ReferenceDataSourceUpdate', 'referenceDataSourceUpdate_s3ReferenceDataSourceUpdate' - Describes the S3 bucket name, object key name, and IAM role that Amazon
-- Kinesis Analytics can assume to read the Amazon S3 object on your behalf
-- and populate the in-application reference table.
--
-- 'referenceSchemaUpdate', 'referenceDataSourceUpdate_referenceSchemaUpdate' - Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns created in the in-application
-- stream.
--
-- 'referenceId', 'referenceDataSourceUpdate_referenceId' - ID of the reference data source being updated. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get this value.
newReferenceDataSourceUpdate ::
  -- | 'referenceId'
  Prelude.Text ->
  ReferenceDataSourceUpdate
newReferenceDataSourceUpdate pReferenceId_ =
  ReferenceDataSourceUpdate'
    { tableNameUpdate =
        Prelude.Nothing,
      s3ReferenceDataSourceUpdate = Prelude.Nothing,
      referenceSchemaUpdate = Prelude.Nothing,
      referenceId = pReferenceId_
    }

-- | In-application table name that is created by this update.
referenceDataSourceUpdate_tableNameUpdate :: Lens.Lens' ReferenceDataSourceUpdate (Prelude.Maybe Prelude.Text)
referenceDataSourceUpdate_tableNameUpdate = Lens.lens (\ReferenceDataSourceUpdate' {tableNameUpdate} -> tableNameUpdate) (\s@ReferenceDataSourceUpdate' {} a -> s {tableNameUpdate = a} :: ReferenceDataSourceUpdate)

-- | Describes the S3 bucket name, object key name, and IAM role that Amazon
-- Kinesis Analytics can assume to read the Amazon S3 object on your behalf
-- and populate the in-application reference table.
referenceDataSourceUpdate_s3ReferenceDataSourceUpdate :: Lens.Lens' ReferenceDataSourceUpdate (Prelude.Maybe S3ReferenceDataSourceUpdate)
referenceDataSourceUpdate_s3ReferenceDataSourceUpdate = Lens.lens (\ReferenceDataSourceUpdate' {s3ReferenceDataSourceUpdate} -> s3ReferenceDataSourceUpdate) (\s@ReferenceDataSourceUpdate' {} a -> s {s3ReferenceDataSourceUpdate = a} :: ReferenceDataSourceUpdate)

-- | Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns created in the in-application
-- stream.
referenceDataSourceUpdate_referenceSchemaUpdate :: Lens.Lens' ReferenceDataSourceUpdate (Prelude.Maybe SourceSchema)
referenceDataSourceUpdate_referenceSchemaUpdate = Lens.lens (\ReferenceDataSourceUpdate' {referenceSchemaUpdate} -> referenceSchemaUpdate) (\s@ReferenceDataSourceUpdate' {} a -> s {referenceSchemaUpdate = a} :: ReferenceDataSourceUpdate)

-- | ID of the reference data source being updated. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get this value.
referenceDataSourceUpdate_referenceId :: Lens.Lens' ReferenceDataSourceUpdate Prelude.Text
referenceDataSourceUpdate_referenceId = Lens.lens (\ReferenceDataSourceUpdate' {referenceId} -> referenceId) (\s@ReferenceDataSourceUpdate' {} a -> s {referenceId = a} :: ReferenceDataSourceUpdate)

instance Prelude.Hashable ReferenceDataSourceUpdate where
  hashWithSalt salt' ReferenceDataSourceUpdate' {..} =
    salt' `Prelude.hashWithSalt` referenceId
      `Prelude.hashWithSalt` referenceSchemaUpdate
      `Prelude.hashWithSalt` s3ReferenceDataSourceUpdate
      `Prelude.hashWithSalt` tableNameUpdate

instance Prelude.NFData ReferenceDataSourceUpdate where
  rnf ReferenceDataSourceUpdate' {..} =
    Prelude.rnf tableNameUpdate
      `Prelude.seq` Prelude.rnf referenceId
      `Prelude.seq` Prelude.rnf referenceSchemaUpdate
      `Prelude.seq` Prelude.rnf s3ReferenceDataSourceUpdate

instance Core.ToJSON ReferenceDataSourceUpdate where
  toJSON ReferenceDataSourceUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TableNameUpdate" Core..=)
              Prelude.<$> tableNameUpdate,
            ("S3ReferenceDataSourceUpdate" Core..=)
              Prelude.<$> s3ReferenceDataSourceUpdate,
            ("ReferenceSchemaUpdate" Core..=)
              Prelude.<$> referenceSchemaUpdate,
            Prelude.Just ("ReferenceId" Core..= referenceId)
          ]
      )
