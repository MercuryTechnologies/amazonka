{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DocumentDB.CopyDBClusterSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies a snapshot of a cluster.
--
-- To copy a cluster snapshot from a shared manual cluster snapshot,
-- @SourceDBClusterSnapshotIdentifier@ must be the Amazon Resource Name
-- (ARN) of the shared cluster snapshot. You can only copy a shared DB
-- cluster snapshot, whether encrypted or not, in the same Region.
--
-- To cancel the copy operation after it is in progress, delete the target
-- cluster snapshot identified by @TargetDBClusterSnapshotIdentifier@ while
-- that cluster snapshot is in the /copying/ status.
module Amazonka.DocumentDB.CopyDBClusterSnapshot
  ( -- * Creating a Request
    CopyDBClusterSnapshot (..),
    newCopyDBClusterSnapshot,

    -- * Request Lenses
    copyDBClusterSnapshot_preSignedUrl,
    copyDBClusterSnapshot_copyTags,
    copyDBClusterSnapshot_kmsKeyId,
    copyDBClusterSnapshot_tags,
    copyDBClusterSnapshot_sourceDBClusterSnapshotIdentifier,
    copyDBClusterSnapshot_targetDBClusterSnapshotIdentifier,

    -- * Destructuring the Response
    CopyDBClusterSnapshotResponse (..),
    newCopyDBClusterSnapshotResponse,

    -- * Response Lenses
    copyDBClusterSnapshotResponse_dbClusterSnapshot,
    copyDBClusterSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DocumentDB.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to CopyDBClusterSnapshot.
--
-- /See:/ 'newCopyDBClusterSnapshot' smart constructor.
data CopyDBClusterSnapshot = CopyDBClusterSnapshot'
  { -- | The URL that contains a Signature Version 4 signed request for
    -- the@CopyDBClusterSnapshot@ API action in the Region that contains the
    -- source cluster snapshot to copy. You must use the @PreSignedUrl@
    -- parameter when copying a cluster snapshot from another Region.
    --
    -- If you are using an Amazon Web Services SDK tool or the CLI, you can
    -- specify @SourceRegion@ (or @--source-region@ for the CLI) instead of
    -- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
    -- autogenerates a pre-signed URL that is a valid request for the operation
    -- that can be executed in the source Region.
    --
    -- The presigned URL must be a valid request for the
    -- @CopyDBClusterSnapshot@ API action that can be executed in the source
    -- Region that contains the cluster snapshot to be copied. The presigned
    -- URL request must contain the following parameter values:
    --
    -- -   @SourceRegion@ - The ID of the region that contains the snapshot to
    --     be copied.
    --
    -- -   @SourceDBClusterSnapshotIdentifier@ - The identifier for the the
    --     encrypted cluster snapshot to be copied. This identifier must be in
    --     the Amazon Resource Name (ARN) format for the source Region. For
    --     example, if you are copying an encrypted cluster snapshot from the
    --     us-east-1 Region, then your @SourceDBClusterSnapshotIdentifier@
    --     looks something like the following:
    --     @arn:aws:rds:us-east-1:12345678012:sample-cluster:sample-cluster-snapshot@.
    --
    -- -   @TargetDBClusterSnapshotIdentifier@ - The identifier for the new
    --     cluster snapshot to be created. This parameter isn\'t case
    --     sensitive.
    preSignedUrl :: Prelude.Maybe Prelude.Text,
    -- | Set to @true@ to copy all tags from the source cluster snapshot to the
    -- target cluster snapshot, and otherwise @false@. The default is @false@.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | The KMS key ID for an encrypted cluster snapshot. The KMS key ID is the
    -- Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for
    -- the KMS encryption key.
    --
    -- If you copy an encrypted cluster snapshot from your account, you can
    -- specify a value for @KmsKeyId@ to encrypt the copy with a new KMS
    -- encryption key. If you don\'t specify a value for @KmsKeyId@, then the
    -- copy of the cluster snapshot is encrypted with the same KMS key as the
    -- source cluster snapshot.
    --
    -- If you copy an encrypted cluster snapshot that is shared from another
    -- account, then you must specify a value for @KmsKeyId@.
    --
    -- To copy an encrypted cluster snapshot to another Region, set @KmsKeyId@
    -- to the KMS key ID that you want to use to encrypt the copy of the
    -- cluster snapshot in the destination Region. KMS encryption keys are
    -- specific to the Region that they are created in, and you can\'t use
    -- encryption keys from one Region in another Region.
    --
    -- If you copy an unencrypted cluster snapshot and specify a value for the
    -- @KmsKeyId@ parameter, an error is returned.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The tags to be assigned to the cluster snapshot.
    tags :: Prelude.Maybe [Tag],
    -- | The identifier of the cluster snapshot to copy. This parameter is not
    -- case sensitive.
    --
    -- Constraints:
    --
    -- -   Must specify a valid system snapshot in the /available/ state.
    --
    -- -   If the source snapshot is in the same Region as the copy, specify a
    --     valid snapshot identifier.
    --
    -- -   If the source snapshot is in a different Region than the copy,
    --     specify a valid cluster snapshot ARN.
    --
    -- Example: @my-cluster-snapshot1@
    sourceDBClusterSnapshotIdentifier :: Prelude.Text,
    -- | The identifier of the new cluster snapshot to create from the source
    -- cluster snapshot. This parameter is not case sensitive.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   The first character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @my-cluster-snapshot2@
    targetDBClusterSnapshotIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyDBClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preSignedUrl', 'copyDBClusterSnapshot_preSignedUrl' - The URL that contains a Signature Version 4 signed request for
-- the@CopyDBClusterSnapshot@ API action in the Region that contains the
-- source cluster snapshot to copy. You must use the @PreSignedUrl@
-- parameter when copying a cluster snapshot from another Region.
--
-- If you are using an Amazon Web Services SDK tool or the CLI, you can
-- specify @SourceRegion@ (or @--source-region@ for the CLI) instead of
-- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
-- autogenerates a pre-signed URL that is a valid request for the operation
-- that can be executed in the source Region.
--
-- The presigned URL must be a valid request for the
-- @CopyDBClusterSnapshot@ API action that can be executed in the source
-- Region that contains the cluster snapshot to be copied. The presigned
-- URL request must contain the following parameter values:
--
-- -   @SourceRegion@ - The ID of the region that contains the snapshot to
--     be copied.
--
-- -   @SourceDBClusterSnapshotIdentifier@ - The identifier for the the
--     encrypted cluster snapshot to be copied. This identifier must be in
--     the Amazon Resource Name (ARN) format for the source Region. For
--     example, if you are copying an encrypted cluster snapshot from the
--     us-east-1 Region, then your @SourceDBClusterSnapshotIdentifier@
--     looks something like the following:
--     @arn:aws:rds:us-east-1:12345678012:sample-cluster:sample-cluster-snapshot@.
--
-- -   @TargetDBClusterSnapshotIdentifier@ - The identifier for the new
--     cluster snapshot to be created. This parameter isn\'t case
--     sensitive.
--
-- 'copyTags', 'copyDBClusterSnapshot_copyTags' - Set to @true@ to copy all tags from the source cluster snapshot to the
-- target cluster snapshot, and otherwise @false@. The default is @false@.
--
-- 'kmsKeyId', 'copyDBClusterSnapshot_kmsKeyId' - The KMS key ID for an encrypted cluster snapshot. The KMS key ID is the
-- Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for
-- the KMS encryption key.
--
-- If you copy an encrypted cluster snapshot from your account, you can
-- specify a value for @KmsKeyId@ to encrypt the copy with a new KMS
-- encryption key. If you don\'t specify a value for @KmsKeyId@, then the
-- copy of the cluster snapshot is encrypted with the same KMS key as the
-- source cluster snapshot.
--
-- If you copy an encrypted cluster snapshot that is shared from another
-- account, then you must specify a value for @KmsKeyId@.
--
-- To copy an encrypted cluster snapshot to another Region, set @KmsKeyId@
-- to the KMS key ID that you want to use to encrypt the copy of the
-- cluster snapshot in the destination Region. KMS encryption keys are
-- specific to the Region that they are created in, and you can\'t use
-- encryption keys from one Region in another Region.
--
-- If you copy an unencrypted cluster snapshot and specify a value for the
-- @KmsKeyId@ parameter, an error is returned.
--
-- 'tags', 'copyDBClusterSnapshot_tags' - The tags to be assigned to the cluster snapshot.
--
-- 'sourceDBClusterSnapshotIdentifier', 'copyDBClusterSnapshot_sourceDBClusterSnapshotIdentifier' - The identifier of the cluster snapshot to copy. This parameter is not
-- case sensitive.
--
-- Constraints:
--
-- -   Must specify a valid system snapshot in the /available/ state.
--
-- -   If the source snapshot is in the same Region as the copy, specify a
--     valid snapshot identifier.
--
-- -   If the source snapshot is in a different Region than the copy,
--     specify a valid cluster snapshot ARN.
--
-- Example: @my-cluster-snapshot1@
--
-- 'targetDBClusterSnapshotIdentifier', 'copyDBClusterSnapshot_targetDBClusterSnapshotIdentifier' - The identifier of the new cluster snapshot to create from the source
-- cluster snapshot. This parameter is not case sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster-snapshot2@
newCopyDBClusterSnapshot ::
  -- | 'sourceDBClusterSnapshotIdentifier'
  Prelude.Text ->
  -- | 'targetDBClusterSnapshotIdentifier'
  Prelude.Text ->
  CopyDBClusterSnapshot
newCopyDBClusterSnapshot
  pSourceDBClusterSnapshotIdentifier_
  pTargetDBClusterSnapshotIdentifier_ =
    CopyDBClusterSnapshot'
      { preSignedUrl =
          Prelude.Nothing,
        copyTags = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        tags = Prelude.Nothing,
        sourceDBClusterSnapshotIdentifier =
          pSourceDBClusterSnapshotIdentifier_,
        targetDBClusterSnapshotIdentifier =
          pTargetDBClusterSnapshotIdentifier_
      }

-- | The URL that contains a Signature Version 4 signed request for
-- the@CopyDBClusterSnapshot@ API action in the Region that contains the
-- source cluster snapshot to copy. You must use the @PreSignedUrl@
-- parameter when copying a cluster snapshot from another Region.
--
-- If you are using an Amazon Web Services SDK tool or the CLI, you can
-- specify @SourceRegion@ (or @--source-region@ for the CLI) instead of
-- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
-- autogenerates a pre-signed URL that is a valid request for the operation
-- that can be executed in the source Region.
--
-- The presigned URL must be a valid request for the
-- @CopyDBClusterSnapshot@ API action that can be executed in the source
-- Region that contains the cluster snapshot to be copied. The presigned
-- URL request must contain the following parameter values:
--
-- -   @SourceRegion@ - The ID of the region that contains the snapshot to
--     be copied.
--
-- -   @SourceDBClusterSnapshotIdentifier@ - The identifier for the the
--     encrypted cluster snapshot to be copied. This identifier must be in
--     the Amazon Resource Name (ARN) format for the source Region. For
--     example, if you are copying an encrypted cluster snapshot from the
--     us-east-1 Region, then your @SourceDBClusterSnapshotIdentifier@
--     looks something like the following:
--     @arn:aws:rds:us-east-1:12345678012:sample-cluster:sample-cluster-snapshot@.
--
-- -   @TargetDBClusterSnapshotIdentifier@ - The identifier for the new
--     cluster snapshot to be created. This parameter isn\'t case
--     sensitive.
copyDBClusterSnapshot_preSignedUrl :: Lens.Lens' CopyDBClusterSnapshot (Prelude.Maybe Prelude.Text)
copyDBClusterSnapshot_preSignedUrl = Lens.lens (\CopyDBClusterSnapshot' {preSignedUrl} -> preSignedUrl) (\s@CopyDBClusterSnapshot' {} a -> s {preSignedUrl = a} :: CopyDBClusterSnapshot)

-- | Set to @true@ to copy all tags from the source cluster snapshot to the
-- target cluster snapshot, and otherwise @false@. The default is @false@.
copyDBClusterSnapshot_copyTags :: Lens.Lens' CopyDBClusterSnapshot (Prelude.Maybe Prelude.Bool)
copyDBClusterSnapshot_copyTags = Lens.lens (\CopyDBClusterSnapshot' {copyTags} -> copyTags) (\s@CopyDBClusterSnapshot' {} a -> s {copyTags = a} :: CopyDBClusterSnapshot)

-- | The KMS key ID for an encrypted cluster snapshot. The KMS key ID is the
-- Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for
-- the KMS encryption key.
--
-- If you copy an encrypted cluster snapshot from your account, you can
-- specify a value for @KmsKeyId@ to encrypt the copy with a new KMS
-- encryption key. If you don\'t specify a value for @KmsKeyId@, then the
-- copy of the cluster snapshot is encrypted with the same KMS key as the
-- source cluster snapshot.
--
-- If you copy an encrypted cluster snapshot that is shared from another
-- account, then you must specify a value for @KmsKeyId@.
--
-- To copy an encrypted cluster snapshot to another Region, set @KmsKeyId@
-- to the KMS key ID that you want to use to encrypt the copy of the
-- cluster snapshot in the destination Region. KMS encryption keys are
-- specific to the Region that they are created in, and you can\'t use
-- encryption keys from one Region in another Region.
--
-- If you copy an unencrypted cluster snapshot and specify a value for the
-- @KmsKeyId@ parameter, an error is returned.
copyDBClusterSnapshot_kmsKeyId :: Lens.Lens' CopyDBClusterSnapshot (Prelude.Maybe Prelude.Text)
copyDBClusterSnapshot_kmsKeyId = Lens.lens (\CopyDBClusterSnapshot' {kmsKeyId} -> kmsKeyId) (\s@CopyDBClusterSnapshot' {} a -> s {kmsKeyId = a} :: CopyDBClusterSnapshot)

-- | The tags to be assigned to the cluster snapshot.
copyDBClusterSnapshot_tags :: Lens.Lens' CopyDBClusterSnapshot (Prelude.Maybe [Tag])
copyDBClusterSnapshot_tags = Lens.lens (\CopyDBClusterSnapshot' {tags} -> tags) (\s@CopyDBClusterSnapshot' {} a -> s {tags = a} :: CopyDBClusterSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the cluster snapshot to copy. This parameter is not
-- case sensitive.
--
-- Constraints:
--
-- -   Must specify a valid system snapshot in the /available/ state.
--
-- -   If the source snapshot is in the same Region as the copy, specify a
--     valid snapshot identifier.
--
-- -   If the source snapshot is in a different Region than the copy,
--     specify a valid cluster snapshot ARN.
--
-- Example: @my-cluster-snapshot1@
copyDBClusterSnapshot_sourceDBClusterSnapshotIdentifier :: Lens.Lens' CopyDBClusterSnapshot Prelude.Text
copyDBClusterSnapshot_sourceDBClusterSnapshotIdentifier = Lens.lens (\CopyDBClusterSnapshot' {sourceDBClusterSnapshotIdentifier} -> sourceDBClusterSnapshotIdentifier) (\s@CopyDBClusterSnapshot' {} a -> s {sourceDBClusterSnapshotIdentifier = a} :: CopyDBClusterSnapshot)

-- | The identifier of the new cluster snapshot to create from the source
-- cluster snapshot. This parameter is not case sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster-snapshot2@
copyDBClusterSnapshot_targetDBClusterSnapshotIdentifier :: Lens.Lens' CopyDBClusterSnapshot Prelude.Text
copyDBClusterSnapshot_targetDBClusterSnapshotIdentifier = Lens.lens (\CopyDBClusterSnapshot' {targetDBClusterSnapshotIdentifier} -> targetDBClusterSnapshotIdentifier) (\s@CopyDBClusterSnapshot' {} a -> s {targetDBClusterSnapshotIdentifier = a} :: CopyDBClusterSnapshot)

instance Core.AWSRequest CopyDBClusterSnapshot where
  type
    AWSResponse CopyDBClusterSnapshot =
      CopyDBClusterSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CopyDBClusterSnapshotResult"
      ( \s h x ->
          CopyDBClusterSnapshotResponse'
            Prelude.<$> (x Core..@? "DBClusterSnapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyDBClusterSnapshot where
  hashWithSalt salt' CopyDBClusterSnapshot' {..} =
    salt'
      `Prelude.hashWithSalt` targetDBClusterSnapshotIdentifier
      `Prelude.hashWithSalt` sourceDBClusterSnapshotIdentifier
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` copyTags
      `Prelude.hashWithSalt` preSignedUrl

instance Prelude.NFData CopyDBClusterSnapshot where
  rnf CopyDBClusterSnapshot' {..} =
    Prelude.rnf preSignedUrl
      `Prelude.seq` Prelude.rnf targetDBClusterSnapshotIdentifier
      `Prelude.seq` Prelude.rnf sourceDBClusterSnapshotIdentifier
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf copyTags

instance Core.ToHeaders CopyDBClusterSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CopyDBClusterSnapshot where
  toPath = Prelude.const "/"

instance Core.ToQuery CopyDBClusterSnapshot where
  toQuery CopyDBClusterSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CopyDBClusterSnapshot" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "PreSignedUrl" Core.=: preSignedUrl,
        "CopyTags" Core.=: copyTags,
        "KmsKeyId" Core.=: kmsKeyId,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "SourceDBClusterSnapshotIdentifier"
          Core.=: sourceDBClusterSnapshotIdentifier,
        "TargetDBClusterSnapshotIdentifier"
          Core.=: targetDBClusterSnapshotIdentifier
      ]

-- | /See:/ 'newCopyDBClusterSnapshotResponse' smart constructor.
data CopyDBClusterSnapshotResponse = CopyDBClusterSnapshotResponse'
  { dbClusterSnapshot :: Prelude.Maybe DBClusterSnapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyDBClusterSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterSnapshot', 'copyDBClusterSnapshotResponse_dbClusterSnapshot' - Undocumented member.
--
-- 'httpStatus', 'copyDBClusterSnapshotResponse_httpStatus' - The response's http status code.
newCopyDBClusterSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopyDBClusterSnapshotResponse
newCopyDBClusterSnapshotResponse pHttpStatus_ =
  CopyDBClusterSnapshotResponse'
    { dbClusterSnapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
copyDBClusterSnapshotResponse_dbClusterSnapshot :: Lens.Lens' CopyDBClusterSnapshotResponse (Prelude.Maybe DBClusterSnapshot)
copyDBClusterSnapshotResponse_dbClusterSnapshot = Lens.lens (\CopyDBClusterSnapshotResponse' {dbClusterSnapshot} -> dbClusterSnapshot) (\s@CopyDBClusterSnapshotResponse' {} a -> s {dbClusterSnapshot = a} :: CopyDBClusterSnapshotResponse)

-- | The response's http status code.
copyDBClusterSnapshotResponse_httpStatus :: Lens.Lens' CopyDBClusterSnapshotResponse Prelude.Int
copyDBClusterSnapshotResponse_httpStatus = Lens.lens (\CopyDBClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@CopyDBClusterSnapshotResponse' {} a -> s {httpStatus = a} :: CopyDBClusterSnapshotResponse)

instance Prelude.NFData CopyDBClusterSnapshotResponse where
  rnf CopyDBClusterSnapshotResponse' {..} =
    Prelude.rnf dbClusterSnapshot
      `Prelude.seq` Prelude.rnf httpStatus
