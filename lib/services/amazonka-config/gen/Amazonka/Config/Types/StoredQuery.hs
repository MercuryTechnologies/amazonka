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
-- Module      : Amazonka.Config.Types.StoredQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.StoredQuery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of a stored query.
--
-- /See:/ 'newStoredQuery' smart constructor.
data StoredQuery = StoredQuery'
  { -- | The ID of the query.
    queryId :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the query. For example,
    -- arn:partition:service:region:account-id:resource-type\/resource-name\/resource-id.
    queryArn :: Prelude.Maybe Prelude.Text,
    -- | The expression of the query. For example,
    -- @SELECT resourceId, resourceType, supplementaryConfiguration.BucketVersioningConfiguration.status WHERE resourceType = \'AWS::S3::Bucket\' AND supplementaryConfiguration.BucketVersioningConfiguration.status = \'Off\'.@
    expression :: Prelude.Maybe Prelude.Text,
    -- | A unique description for the query.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the query.
    queryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StoredQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryId', 'storedQuery_queryId' - The ID of the query.
--
-- 'queryArn', 'storedQuery_queryArn' - Amazon Resource Name (ARN) of the query. For example,
-- arn:partition:service:region:account-id:resource-type\/resource-name\/resource-id.
--
-- 'expression', 'storedQuery_expression' - The expression of the query. For example,
-- @SELECT resourceId, resourceType, supplementaryConfiguration.BucketVersioningConfiguration.status WHERE resourceType = \'AWS::S3::Bucket\' AND supplementaryConfiguration.BucketVersioningConfiguration.status = \'Off\'.@
--
-- 'description', 'storedQuery_description' - A unique description for the query.
--
-- 'queryName', 'storedQuery_queryName' - The name of the query.
newStoredQuery ::
  -- | 'queryName'
  Prelude.Text ->
  StoredQuery
newStoredQuery pQueryName_ =
  StoredQuery'
    { queryId = Prelude.Nothing,
      queryArn = Prelude.Nothing,
      expression = Prelude.Nothing,
      description = Prelude.Nothing,
      queryName = pQueryName_
    }

-- | The ID of the query.
storedQuery_queryId :: Lens.Lens' StoredQuery (Prelude.Maybe Prelude.Text)
storedQuery_queryId = Lens.lens (\StoredQuery' {queryId} -> queryId) (\s@StoredQuery' {} a -> s {queryId = a} :: StoredQuery)

-- | Amazon Resource Name (ARN) of the query. For example,
-- arn:partition:service:region:account-id:resource-type\/resource-name\/resource-id.
storedQuery_queryArn :: Lens.Lens' StoredQuery (Prelude.Maybe Prelude.Text)
storedQuery_queryArn = Lens.lens (\StoredQuery' {queryArn} -> queryArn) (\s@StoredQuery' {} a -> s {queryArn = a} :: StoredQuery)

-- | The expression of the query. For example,
-- @SELECT resourceId, resourceType, supplementaryConfiguration.BucketVersioningConfiguration.status WHERE resourceType = \'AWS::S3::Bucket\' AND supplementaryConfiguration.BucketVersioningConfiguration.status = \'Off\'.@
storedQuery_expression :: Lens.Lens' StoredQuery (Prelude.Maybe Prelude.Text)
storedQuery_expression = Lens.lens (\StoredQuery' {expression} -> expression) (\s@StoredQuery' {} a -> s {expression = a} :: StoredQuery)

-- | A unique description for the query.
storedQuery_description :: Lens.Lens' StoredQuery (Prelude.Maybe Prelude.Text)
storedQuery_description = Lens.lens (\StoredQuery' {description} -> description) (\s@StoredQuery' {} a -> s {description = a} :: StoredQuery)

-- | The name of the query.
storedQuery_queryName :: Lens.Lens' StoredQuery Prelude.Text
storedQuery_queryName = Lens.lens (\StoredQuery' {queryName} -> queryName) (\s@StoredQuery' {} a -> s {queryName = a} :: StoredQuery)

instance Core.FromJSON StoredQuery where
  parseJSON =
    Core.withObject
      "StoredQuery"
      ( \x ->
          StoredQuery'
            Prelude.<$> (x Core..:? "QueryId")
            Prelude.<*> (x Core..:? "QueryArn")
            Prelude.<*> (x Core..:? "Expression")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..: "QueryName")
      )

instance Prelude.Hashable StoredQuery where
  hashWithSalt salt' StoredQuery' {..} =
    salt' `Prelude.hashWithSalt` queryName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` queryArn
      `Prelude.hashWithSalt` queryId

instance Prelude.NFData StoredQuery where
  rnf StoredQuery' {..} =
    Prelude.rnf queryId
      `Prelude.seq` Prelude.rnf queryName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf queryArn

instance Core.ToJSON StoredQuery where
  toJSON StoredQuery' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("QueryId" Core..=) Prelude.<$> queryId,
            ("QueryArn" Core..=) Prelude.<$> queryArn,
            ("Expression" Core..=) Prelude.<$> expression,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("QueryName" Core..= queryName)
          ]
      )
