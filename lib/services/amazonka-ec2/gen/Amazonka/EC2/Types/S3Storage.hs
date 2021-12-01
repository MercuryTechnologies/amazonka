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
-- Module      : Amazonka.EC2.Types.S3Storage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.S3Storage where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the storage parameters for Amazon S3 and Amazon S3 buckets for
-- an instance store-backed AMI.
--
-- /See:/ 'newS3Storage' smart constructor.
data S3Storage = S3Storage'
  { -- | The beginning of the file name of the AMI.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | An Amazon S3 upload policy that gives Amazon EC2 permission to upload
    -- items into Amazon S3 on your behalf.
    uploadPolicy :: Prelude.Maybe Core.Base64,
    -- | The bucket in which to store the AMI. You can specify a bucket that you
    -- already own or a new bucket that Amazon EC2 creates on your behalf. If
    -- you specify a bucket that belongs to someone else, Amazon EC2 returns an
    -- error.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The signature of the JSON document.
    uploadPolicySignature :: Prelude.Maybe Prelude.Text,
    -- | The access key ID of the owner of the bucket. Before you specify a value
    -- for your access key ID, review and follow the guidance in
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html Best Practices for Managing Amazon Web Services Access Keys>.
    aWSAccessKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Storage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 's3Storage_prefix' - The beginning of the file name of the AMI.
--
-- 'uploadPolicy', 's3Storage_uploadPolicy' - An Amazon S3 upload policy that gives Amazon EC2 permission to upload
-- items into Amazon S3 on your behalf.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'bucket', 's3Storage_bucket' - The bucket in which to store the AMI. You can specify a bucket that you
-- already own or a new bucket that Amazon EC2 creates on your behalf. If
-- you specify a bucket that belongs to someone else, Amazon EC2 returns an
-- error.
--
-- 'uploadPolicySignature', 's3Storage_uploadPolicySignature' - The signature of the JSON document.
--
-- 'aWSAccessKeyId', 's3Storage_aWSAccessKeyId' - The access key ID of the owner of the bucket. Before you specify a value
-- for your access key ID, review and follow the guidance in
-- <https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html Best Practices for Managing Amazon Web Services Access Keys>.
newS3Storage ::
  S3Storage
newS3Storage =
  S3Storage'
    { prefix = Prelude.Nothing,
      uploadPolicy = Prelude.Nothing,
      bucket = Prelude.Nothing,
      uploadPolicySignature = Prelude.Nothing,
      aWSAccessKeyId = Prelude.Nothing
    }

-- | The beginning of the file name of the AMI.
s3Storage_prefix :: Lens.Lens' S3Storage (Prelude.Maybe Prelude.Text)
s3Storage_prefix = Lens.lens (\S3Storage' {prefix} -> prefix) (\s@S3Storage' {} a -> s {prefix = a} :: S3Storage)

-- | An Amazon S3 upload policy that gives Amazon EC2 permission to upload
-- items into Amazon S3 on your behalf.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
s3Storage_uploadPolicy :: Lens.Lens' S3Storage (Prelude.Maybe Prelude.ByteString)
s3Storage_uploadPolicy = Lens.lens (\S3Storage' {uploadPolicy} -> uploadPolicy) (\s@S3Storage' {} a -> s {uploadPolicy = a} :: S3Storage) Prelude.. Lens.mapping Core._Base64

-- | The bucket in which to store the AMI. You can specify a bucket that you
-- already own or a new bucket that Amazon EC2 creates on your behalf. If
-- you specify a bucket that belongs to someone else, Amazon EC2 returns an
-- error.
s3Storage_bucket :: Lens.Lens' S3Storage (Prelude.Maybe Prelude.Text)
s3Storage_bucket = Lens.lens (\S3Storage' {bucket} -> bucket) (\s@S3Storage' {} a -> s {bucket = a} :: S3Storage)

-- | The signature of the JSON document.
s3Storage_uploadPolicySignature :: Lens.Lens' S3Storage (Prelude.Maybe Prelude.Text)
s3Storage_uploadPolicySignature = Lens.lens (\S3Storage' {uploadPolicySignature} -> uploadPolicySignature) (\s@S3Storage' {} a -> s {uploadPolicySignature = a} :: S3Storage)

-- | The access key ID of the owner of the bucket. Before you specify a value
-- for your access key ID, review and follow the guidance in
-- <https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html Best Practices for Managing Amazon Web Services Access Keys>.
s3Storage_aWSAccessKeyId :: Lens.Lens' S3Storage (Prelude.Maybe Prelude.Text)
s3Storage_aWSAccessKeyId = Lens.lens (\S3Storage' {aWSAccessKeyId} -> aWSAccessKeyId) (\s@S3Storage' {} a -> s {aWSAccessKeyId = a} :: S3Storage)

instance Core.FromXML S3Storage where
  parseXML x =
    S3Storage'
      Prelude.<$> (x Core..@? "prefix")
      Prelude.<*> (x Core..@? "uploadPolicy")
      Prelude.<*> (x Core..@? "bucket")
      Prelude.<*> (x Core..@? "uploadPolicySignature")
      Prelude.<*> (x Core..@? "AWSAccessKeyId")

instance Prelude.Hashable S3Storage where
  hashWithSalt salt' S3Storage' {..} =
    salt' `Prelude.hashWithSalt` aWSAccessKeyId
      `Prelude.hashWithSalt` uploadPolicySignature
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` uploadPolicy
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData S3Storage where
  rnf S3Storage' {..} =
    Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf aWSAccessKeyId
      `Prelude.seq` Prelude.rnf uploadPolicySignature
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf uploadPolicy

instance Core.ToQuery S3Storage where
  toQuery S3Storage' {..} =
    Prelude.mconcat
      [ "Prefix" Core.=: prefix,
        "UploadPolicy" Core.=: uploadPolicy,
        "Bucket" Core.=: bucket,
        "UploadPolicySignature"
          Core.=: uploadPolicySignature,
        "AWSAccessKeyId" Core.=: aWSAccessKeyId
      ]
