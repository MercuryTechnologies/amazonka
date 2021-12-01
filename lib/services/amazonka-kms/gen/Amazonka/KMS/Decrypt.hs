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
-- Module      : Amazonka.KMS.Decrypt
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decrypts ciphertext that was encrypted by a KMS key using any of the
-- following operations:
--
-- -   Encrypt
--
-- -   GenerateDataKey
--
-- -   GenerateDataKeyPair
--
-- -   GenerateDataKeyWithoutPlaintext
--
-- -   GenerateDataKeyPairWithoutPlaintext
--
-- You can use this operation to decrypt ciphertext that was encrypted
-- under a symmetric or asymmetric KMS key. When the KMS key is asymmetric,
-- you must specify the KMS key and the encryption algorithm that was used
-- to encrypt the ciphertext. For information about symmetric and
-- asymmetric KMS keys, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- The Decrypt operation also decrypts ciphertext that was encrypted
-- outside of KMS by the public key in an KMS asymmetric KMS key. However,
-- it cannot decrypt ciphertext produced by other libraries, such as the
-- <https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/ Amazon Web Services Encryption SDK>
-- or
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 client-side encryption>.
-- These libraries return a ciphertext format that is incompatible with
-- KMS.
--
-- If the ciphertext was encrypted under a symmetric KMS key, the @KeyId@
-- parameter is optional. KMS can get this information from metadata that
-- it adds to the symmetric ciphertext blob. This feature adds durability
-- to your implementation by ensuring that authorized users can decrypt
-- ciphertext decades after it was encrypted, even if they\'ve lost track
-- of the key ID. However, specifying the KMS key is always recommended as
-- a best practice. When you use the @KeyId@ parameter to specify a KMS
-- key, KMS only uses the KMS key you specify. If the ciphertext was
-- encrypted under a different KMS key, the @Decrypt@ operation fails. This
-- practice ensures that you use the KMS key that you intend.
--
-- Whenever possible, use key policies to give users permission to call the
-- @Decrypt@ operation on a particular KMS key, instead of using IAM
-- policies. Otherwise, you might create an IAM user policy that gives the
-- user @Decrypt@ permission on all KMS keys. This user could decrypt
-- ciphertext that was encrypted by KMS keys in other accounts if the key
-- policy for the cross-account KMS key permits it. If you must use an IAM
-- policy for @Decrypt@ permissions, limit the user to particular KMS keys
-- or particular trusted accounts. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/iam-policies.html#iam-policies-best-practices Best practices for IAM policies>
-- in the /Key Management Service Developer Guide/.
--
-- Applications in Amazon Web Services Nitro Enclaves can call this
-- operation by using the
-- <https://github.com/aws/aws-nitro-enclaves-sdk-c Amazon Web Services Nitro Enclaves Development Kit>.
-- For information about the supporting parameters, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/services-nitro-enclaves.html How Amazon Web Services Nitro Enclaves use KMS>
-- in the /Key Management Service Developer Guide/.
--
-- The KMS key that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key state: Effect on your KMS key>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: Yes. To perform this operation with a KMS key in
-- a different Amazon Web Services account, specify the key ARN or alias
-- ARN in the value of the @KeyId@ parameter.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:Decrypt>
-- (key policy)
--
-- __Related operations:__
--
-- -   Encrypt
--
-- -   GenerateDataKey
--
-- -   GenerateDataKeyPair
--
-- -   ReEncrypt
module Amazonka.KMS.Decrypt
  ( -- * Creating a Request
    Decrypt (..),
    newDecrypt,

    -- * Request Lenses
    decrypt_keyId,
    decrypt_encryptionContext,
    decrypt_grantTokens,
    decrypt_encryptionAlgorithm,
    decrypt_ciphertextBlob,

    -- * Destructuring the Response
    DecryptResponse (..),
    newDecryptResponse,

    -- * Response Lenses
    decryptResponse_keyId,
    decryptResponse_plaintext,
    decryptResponse_encryptionAlgorithm,
    decryptResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.KMS.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDecrypt' smart constructor.
data Decrypt = Decrypt'
  { -- | Specifies the KMS key that KMS uses to decrypt the ciphertext. Enter a
    -- key ID of the KMS key that was used to encrypt the ciphertext.
    --
    -- This parameter is required only when the ciphertext was encrypted under
    -- an asymmetric KMS key. If you used a symmetric KMS key, KMS can get the
    -- KMS key from metadata that it adds to the symmetric ciphertext blob.
    -- However, it is always recommended as a best practice. This practice
    -- ensures that you use the KMS key that you intend.
    --
    -- To specify a KMS key, use its key ID, key ARN, alias name, or alias ARN.
    -- When using an alias name, prefix it with @\"alias\/\"@. To specify a KMS
    -- key in a different Amazon Web Services account, you must use the key ARN
    -- or alias ARN.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Alias name: @alias\/ExampleAlias@
    --
    -- -   Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias@
    --
    -- To get the key ID and key ARN for a KMS key, use ListKeys or
    -- DescribeKey. To get the alias name and alias ARN, use ListAliases.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the encryption context to use when decrypting the data. An
    -- encryption context is valid only for
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
    -- with a symmetric KMS key. The standard asymmetric encryption algorithms
    -- that KMS uses do not support an encryption context.
    --
    -- An /encryption context/ is a collection of non-secret key-value pairs
    -- that represents additional authenticated data. When you use an
    -- encryption context to encrypt data, you must specify the same (an exact
    -- case-sensitive match) encryption context to decrypt the data. An
    -- encryption context is optional when encrypting with a symmetric KMS key,
    -- but it is highly recommended.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context>
    -- in the /Key Management Service Developer Guide/.
    encryptionContext :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of grant tokens.
    --
    -- Use a grant token when your permission to call this operation comes from
    -- a new grant that has not yet achieved /eventual consistency/. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
    -- and
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
    -- in the /Key Management Service Developer Guide/.
    grantTokens :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the encryption algorithm that will be used to decrypt the
    -- ciphertext. Specify the same algorithm that was used to encrypt the
    -- data. If you specify a different algorithm, the @Decrypt@ operation
    -- fails.
    --
    -- This parameter is required only when the ciphertext was encrypted under
    -- an asymmetric KMS key. The default value, @SYMMETRIC_DEFAULT@,
    -- represents the only supported algorithm that is valid for symmetric KMS
    -- keys.
    encryptionAlgorithm :: Prelude.Maybe EncryptionAlgorithmSpec,
    -- | Ciphertext to be decrypted. The blob includes metadata.
    ciphertextBlob :: Core.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Decrypt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'decrypt_keyId' - Specifies the KMS key that KMS uses to decrypt the ciphertext. Enter a
-- key ID of the KMS key that was used to encrypt the ciphertext.
--
-- This parameter is required only when the ciphertext was encrypted under
-- an asymmetric KMS key. If you used a symmetric KMS key, KMS can get the
-- KMS key from metadata that it adds to the symmetric ciphertext blob.
-- However, it is always recommended as a best practice. This practice
-- ensures that you use the KMS key that you intend.
--
-- To specify a KMS key, use its key ID, key ARN, alias name, or alias ARN.
-- When using an alias name, prefix it with @\"alias\/\"@. To specify a KMS
-- key in a different Amazon Web Services account, you must use the key ARN
-- or alias ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey. To get the alias name and alias ARN, use ListAliases.
--
-- 'encryptionContext', 'decrypt_encryptionContext' - Specifies the encryption context to use when decrypting the data. An
-- encryption context is valid only for
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- with a symmetric KMS key. The standard asymmetric encryption algorithms
-- that KMS uses do not support an encryption context.
--
-- An /encryption context/ is a collection of non-secret key-value pairs
-- that represents additional authenticated data. When you use an
-- encryption context to encrypt data, you must specify the same (an exact
-- case-sensitive match) encryption context to decrypt the data. An
-- encryption context is optional when encrypting with a symmetric KMS key,
-- but it is highly recommended.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context>
-- in the /Key Management Service Developer Guide/.
--
-- 'grantTokens', 'decrypt_grantTokens' - A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
--
-- 'encryptionAlgorithm', 'decrypt_encryptionAlgorithm' - Specifies the encryption algorithm that will be used to decrypt the
-- ciphertext. Specify the same algorithm that was used to encrypt the
-- data. If you specify a different algorithm, the @Decrypt@ operation
-- fails.
--
-- This parameter is required only when the ciphertext was encrypted under
-- an asymmetric KMS key. The default value, @SYMMETRIC_DEFAULT@,
-- represents the only supported algorithm that is valid for symmetric KMS
-- keys.
--
-- 'ciphertextBlob', 'decrypt_ciphertextBlob' - Ciphertext to be decrypted. The blob includes metadata.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newDecrypt ::
  -- | 'ciphertextBlob'
  Prelude.ByteString ->
  Decrypt
newDecrypt pCiphertextBlob_ =
  Decrypt'
    { keyId = Prelude.Nothing,
      encryptionContext = Prelude.Nothing,
      grantTokens = Prelude.Nothing,
      encryptionAlgorithm = Prelude.Nothing,
      ciphertextBlob =
        Core._Base64 Lens.# pCiphertextBlob_
    }

-- | Specifies the KMS key that KMS uses to decrypt the ciphertext. Enter a
-- key ID of the KMS key that was used to encrypt the ciphertext.
--
-- This parameter is required only when the ciphertext was encrypted under
-- an asymmetric KMS key. If you used a symmetric KMS key, KMS can get the
-- KMS key from metadata that it adds to the symmetric ciphertext blob.
-- However, it is always recommended as a best practice. This practice
-- ensures that you use the KMS key that you intend.
--
-- To specify a KMS key, use its key ID, key ARN, alias name, or alias ARN.
-- When using an alias name, prefix it with @\"alias\/\"@. To specify a KMS
-- key in a different Amazon Web Services account, you must use the key ARN
-- or alias ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey. To get the alias name and alias ARN, use ListAliases.
decrypt_keyId :: Lens.Lens' Decrypt (Prelude.Maybe Prelude.Text)
decrypt_keyId = Lens.lens (\Decrypt' {keyId} -> keyId) (\s@Decrypt' {} a -> s {keyId = a} :: Decrypt)

-- | Specifies the encryption context to use when decrypting the data. An
-- encryption context is valid only for
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations>
-- with a symmetric KMS key. The standard asymmetric encryption algorithms
-- that KMS uses do not support an encryption context.
--
-- An /encryption context/ is a collection of non-secret key-value pairs
-- that represents additional authenticated data. When you use an
-- encryption context to encrypt data, you must specify the same (an exact
-- case-sensitive match) encryption context to decrypt the data. An
-- encryption context is optional when encrypting with a symmetric KMS key,
-- but it is highly recommended.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context>
-- in the /Key Management Service Developer Guide/.
decrypt_encryptionContext :: Lens.Lens' Decrypt (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
decrypt_encryptionContext = Lens.lens (\Decrypt' {encryptionContext} -> encryptionContext) (\s@Decrypt' {} a -> s {encryptionContext = a} :: Decrypt) Prelude.. Lens.mapping Lens.coerced

-- | A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
decrypt_grantTokens :: Lens.Lens' Decrypt (Prelude.Maybe [Prelude.Text])
decrypt_grantTokens = Lens.lens (\Decrypt' {grantTokens} -> grantTokens) (\s@Decrypt' {} a -> s {grantTokens = a} :: Decrypt) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the encryption algorithm that will be used to decrypt the
-- ciphertext. Specify the same algorithm that was used to encrypt the
-- data. If you specify a different algorithm, the @Decrypt@ operation
-- fails.
--
-- This parameter is required only when the ciphertext was encrypted under
-- an asymmetric KMS key. The default value, @SYMMETRIC_DEFAULT@,
-- represents the only supported algorithm that is valid for symmetric KMS
-- keys.
decrypt_encryptionAlgorithm :: Lens.Lens' Decrypt (Prelude.Maybe EncryptionAlgorithmSpec)
decrypt_encryptionAlgorithm = Lens.lens (\Decrypt' {encryptionAlgorithm} -> encryptionAlgorithm) (\s@Decrypt' {} a -> s {encryptionAlgorithm = a} :: Decrypt)

-- | Ciphertext to be decrypted. The blob includes metadata.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
decrypt_ciphertextBlob :: Lens.Lens' Decrypt Prelude.ByteString
decrypt_ciphertextBlob = Lens.lens (\Decrypt' {ciphertextBlob} -> ciphertextBlob) (\s@Decrypt' {} a -> s {ciphertextBlob = a} :: Decrypt) Prelude.. Core._Base64

instance Core.AWSRequest Decrypt where
  type AWSResponse Decrypt = DecryptResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DecryptResponse'
            Prelude.<$> (x Core..?> "KeyId")
            Prelude.<*> (x Core..?> "Plaintext")
            Prelude.<*> (x Core..?> "EncryptionAlgorithm")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Decrypt where
  hashWithSalt salt' Decrypt' {..} =
    salt' `Prelude.hashWithSalt` ciphertextBlob
      `Prelude.hashWithSalt` encryptionAlgorithm
      `Prelude.hashWithSalt` grantTokens
      `Prelude.hashWithSalt` encryptionContext
      `Prelude.hashWithSalt` keyId

instance Prelude.NFData Decrypt where
  rnf Decrypt' {..} =
    Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf ciphertextBlob
      `Prelude.seq` Prelude.rnf encryptionAlgorithm
      `Prelude.seq` Prelude.rnf grantTokens
      `Prelude.seq` Prelude.rnf encryptionContext

instance Core.ToHeaders Decrypt where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("TrentService.Decrypt" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON Decrypt where
  toJSON Decrypt' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KeyId" Core..=) Prelude.<$> keyId,
            ("EncryptionContext" Core..=)
              Prelude.<$> encryptionContext,
            ("GrantTokens" Core..=) Prelude.<$> grantTokens,
            ("EncryptionAlgorithm" Core..=)
              Prelude.<$> encryptionAlgorithm,
            Prelude.Just
              ("CiphertextBlob" Core..= ciphertextBlob)
          ]
      )

instance Core.ToPath Decrypt where
  toPath = Prelude.const "/"

instance Core.ToQuery Decrypt where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDecryptResponse' smart constructor.
data DecryptResponse = DecryptResponse'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the KMS key that was used to decrypt the ciphertext.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | Decrypted plaintext data. When you use the HTTP API or the Amazon Web
    -- Services CLI, the value is Base64-encoded. Otherwise, it is not
    -- Base64-encoded.
    plaintext :: Prelude.Maybe (Core.Sensitive Core.Base64),
    -- | The encryption algorithm that was used to decrypt the ciphertext.
    encryptionAlgorithm :: Prelude.Maybe EncryptionAlgorithmSpec,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecryptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'decryptResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the KMS key that was used to decrypt the ciphertext.
--
-- 'plaintext', 'decryptResponse_plaintext' - Decrypted plaintext data. When you use the HTTP API or the Amazon Web
-- Services CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'encryptionAlgorithm', 'decryptResponse_encryptionAlgorithm' - The encryption algorithm that was used to decrypt the ciphertext.
--
-- 'httpStatus', 'decryptResponse_httpStatus' - The response's http status code.
newDecryptResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DecryptResponse
newDecryptResponse pHttpStatus_ =
  DecryptResponse'
    { keyId = Prelude.Nothing,
      plaintext = Prelude.Nothing,
      encryptionAlgorithm = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the KMS key that was used to decrypt the ciphertext.
decryptResponse_keyId :: Lens.Lens' DecryptResponse (Prelude.Maybe Prelude.Text)
decryptResponse_keyId = Lens.lens (\DecryptResponse' {keyId} -> keyId) (\s@DecryptResponse' {} a -> s {keyId = a} :: DecryptResponse)

-- | Decrypted plaintext data. When you use the HTTP API or the Amazon Web
-- Services CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
decryptResponse_plaintext :: Lens.Lens' DecryptResponse (Prelude.Maybe Prelude.ByteString)
decryptResponse_plaintext = Lens.lens (\DecryptResponse' {plaintext} -> plaintext) (\s@DecryptResponse' {} a -> s {plaintext = a} :: DecryptResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Core._Base64)

-- | The encryption algorithm that was used to decrypt the ciphertext.
decryptResponse_encryptionAlgorithm :: Lens.Lens' DecryptResponse (Prelude.Maybe EncryptionAlgorithmSpec)
decryptResponse_encryptionAlgorithm = Lens.lens (\DecryptResponse' {encryptionAlgorithm} -> encryptionAlgorithm) (\s@DecryptResponse' {} a -> s {encryptionAlgorithm = a} :: DecryptResponse)

-- | The response's http status code.
decryptResponse_httpStatus :: Lens.Lens' DecryptResponse Prelude.Int
decryptResponse_httpStatus = Lens.lens (\DecryptResponse' {httpStatus} -> httpStatus) (\s@DecryptResponse' {} a -> s {httpStatus = a} :: DecryptResponse)

instance Prelude.NFData DecryptResponse where
  rnf DecryptResponse' {..} =
    Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf encryptionAlgorithm
      `Prelude.seq` Prelude.rnf plaintext
