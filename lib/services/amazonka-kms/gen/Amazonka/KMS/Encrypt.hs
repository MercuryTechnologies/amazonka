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
-- Module      : Amazonka.KMS.Encrypt
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Encrypts plaintext into ciphertext by using a KMS key. The @Encrypt@
-- operation has two primary use cases:
--
-- -   You can encrypt small amounts of arbitrary data, such as a personal
--     identifier or database password, or other sensitive information.
--
-- -   You can use the @Encrypt@ operation to move encrypted data from one
--     Amazon Web Services Region to another. For example, in Region A,
--     generate a data key and use the plaintext key to encrypt your data.
--     Then, in Region A, use the @Encrypt@ operation to encrypt the
--     plaintext data key under a KMS key in Region B. Now, you can move
--     the encrypted data and the encrypted data key to Region B. When
--     necessary, you can decrypt the encrypted data key and the encrypted
--     data entirely within in Region B.
--
-- You don\'t need to use the @Encrypt@ operation to encrypt a data key.
-- The GenerateDataKey and GenerateDataKeyPair operations return a
-- plaintext data key and an encrypted copy of that data key.
--
-- When you encrypt data, you must specify a symmetric or asymmetric KMS
-- key to use in the encryption operation. The KMS key must have a
-- @KeyUsage@ value of @ENCRYPT_DECRYPT.@ To find the @KeyUsage@ of a KMS
-- key, use the DescribeKey operation.
--
-- If you use a symmetric KMS key, you can use an encryption context to add
-- additional security to your encryption operation. If you specify an
-- @EncryptionContext@ when encrypting data, you must specify the same
-- encryption context (a case-sensitive exact match) when decrypting the
-- data. Otherwise, the request to decrypt fails with an
-- @InvalidCiphertextException@. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context>
-- in the /Key Management Service Developer Guide/.
--
-- If you specify an asymmetric KMS key, you must also specify the
-- encryption algorithm. The algorithm must be compatible with the KMS key
-- type.
--
-- When you use an asymmetric KMS key to encrypt or reencrypt data, be sure
-- to record the KMS key and encryption algorithm that you choose. You will
-- be required to provide the same KMS key and encryption algorithm when
-- you decrypt the data. If the KMS key and algorithm do not match the
-- values used to encrypt the data, the decrypt operation fails.
--
-- You are not required to supply the key ID and encryption algorithm when
-- you decrypt with symmetric KMS keys because KMS stores this information
-- in the ciphertext blob. KMS cannot store metadata in ciphertext
-- generated with asymmetric keys. The standard format for asymmetric key
-- ciphertext does not include configurable fields.
--
-- The maximum size of the data that you can encrypt varies with the type
-- of KMS key and the encryption algorithm that you choose.
--
-- -   Symmetric KMS keys
--
--     -   @SYMMETRIC_DEFAULT@: 4096 bytes
--
-- -   @RSA_2048@
--
--     -   @RSAES_OAEP_SHA_1@: 214 bytes
--
--     -   @RSAES_OAEP_SHA_256@: 190 bytes
--
-- -   @RSA_3072@
--
--     -   @RSAES_OAEP_SHA_1@: 342 bytes
--
--     -   @RSAES_OAEP_SHA_256@: 318 bytes
--
-- -   @RSA_4096@
--
--     -   @RSAES_OAEP_SHA_1@: 470 bytes
--
--     -   @RSAES_OAEP_SHA_256@: 446 bytes
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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:Encrypt>
-- (key policy)
--
-- __Related operations:__
--
-- -   Decrypt
--
-- -   GenerateDataKey
--
-- -   GenerateDataKeyPair
module Amazonka.KMS.Encrypt
  ( -- * Creating a Request
    Encrypt (..),
    newEncrypt,

    -- * Request Lenses
    encrypt_encryptionContext,
    encrypt_grantTokens,
    encrypt_encryptionAlgorithm,
    encrypt_keyId,
    encrypt_plaintext,

    -- * Destructuring the Response
    EncryptResponse (..),
    newEncryptResponse,

    -- * Response Lenses
    encryptResponse_keyId,
    encryptResponse_encryptionAlgorithm,
    encryptResponse_ciphertextBlob,
    encryptResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.KMS.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEncrypt' smart constructor.
data Encrypt = Encrypt'
  { -- | Specifies the encryption context that will be used to encrypt the data.
    -- An encryption context is valid only for
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
    -- | Specifies the encryption algorithm that KMS will use to encrypt the
    -- plaintext message. The algorithm must be compatible with the KMS key
    -- that you specify.
    --
    -- This parameter is required only for asymmetric KMS keys. The default
    -- value, @SYMMETRIC_DEFAULT@, is the algorithm used for symmetric KMS
    -- keys. If you are using an asymmetric KMS key, we recommend
    -- RSAES_OAEP_SHA_256.
    encryptionAlgorithm :: Prelude.Maybe EncryptionAlgorithmSpec,
    -- | Identifies the KMS key to use in the encryption operation.
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
    keyId :: Prelude.Text,
    -- | Data to be encrypted.
    plaintext :: Core.Sensitive Core.Base64
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Encrypt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionContext', 'encrypt_encryptionContext' - Specifies the encryption context that will be used to encrypt the data.
-- An encryption context is valid only for
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
-- 'grantTokens', 'encrypt_grantTokens' - A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
--
-- 'encryptionAlgorithm', 'encrypt_encryptionAlgorithm' - Specifies the encryption algorithm that KMS will use to encrypt the
-- plaintext message. The algorithm must be compatible with the KMS key
-- that you specify.
--
-- This parameter is required only for asymmetric KMS keys. The default
-- value, @SYMMETRIC_DEFAULT@, is the algorithm used for symmetric KMS
-- keys. If you are using an asymmetric KMS key, we recommend
-- RSAES_OAEP_SHA_256.
--
-- 'keyId', 'encrypt_keyId' - Identifies the KMS key to use in the encryption operation.
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
-- 'plaintext', 'encrypt_plaintext' - Data to be encrypted.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newEncrypt ::
  -- | 'keyId'
  Prelude.Text ->
  -- | 'plaintext'
  Prelude.ByteString ->
  Encrypt
newEncrypt pKeyId_ pPlaintext_ =
  Encrypt'
    { encryptionContext = Prelude.Nothing,
      grantTokens = Prelude.Nothing,
      encryptionAlgorithm = Prelude.Nothing,
      keyId = pKeyId_,
      plaintext =
        Core._Sensitive Prelude.. Core._Base64
          Lens.# pPlaintext_
    }

-- | Specifies the encryption context that will be used to encrypt the data.
-- An encryption context is valid only for
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
encrypt_encryptionContext :: Lens.Lens' Encrypt (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
encrypt_encryptionContext = Lens.lens (\Encrypt' {encryptionContext} -> encryptionContext) (\s@Encrypt' {} a -> s {encryptionContext = a} :: Encrypt) Prelude.. Lens.mapping Lens.coerced

-- | A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
encrypt_grantTokens :: Lens.Lens' Encrypt (Prelude.Maybe [Prelude.Text])
encrypt_grantTokens = Lens.lens (\Encrypt' {grantTokens} -> grantTokens) (\s@Encrypt' {} a -> s {grantTokens = a} :: Encrypt) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the encryption algorithm that KMS will use to encrypt the
-- plaintext message. The algorithm must be compatible with the KMS key
-- that you specify.
--
-- This parameter is required only for asymmetric KMS keys. The default
-- value, @SYMMETRIC_DEFAULT@, is the algorithm used for symmetric KMS
-- keys. If you are using an asymmetric KMS key, we recommend
-- RSAES_OAEP_SHA_256.
encrypt_encryptionAlgorithm :: Lens.Lens' Encrypt (Prelude.Maybe EncryptionAlgorithmSpec)
encrypt_encryptionAlgorithm = Lens.lens (\Encrypt' {encryptionAlgorithm} -> encryptionAlgorithm) (\s@Encrypt' {} a -> s {encryptionAlgorithm = a} :: Encrypt)

-- | Identifies the KMS key to use in the encryption operation.
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
encrypt_keyId :: Lens.Lens' Encrypt Prelude.Text
encrypt_keyId = Lens.lens (\Encrypt' {keyId} -> keyId) (\s@Encrypt' {} a -> s {keyId = a} :: Encrypt)

-- | Data to be encrypted.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
encrypt_plaintext :: Lens.Lens' Encrypt Prelude.ByteString
encrypt_plaintext = Lens.lens (\Encrypt' {plaintext} -> plaintext) (\s@Encrypt' {} a -> s {plaintext = a} :: Encrypt) Prelude.. Core._Sensitive Prelude.. Core._Base64

instance Core.AWSRequest Encrypt where
  type AWSResponse Encrypt = EncryptResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          EncryptResponse'
            Prelude.<$> (x Core..?> "KeyId")
            Prelude.<*> (x Core..?> "EncryptionAlgorithm")
            Prelude.<*> (x Core..?> "CiphertextBlob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Encrypt where
  hashWithSalt salt' Encrypt' {..} =
    salt' `Prelude.hashWithSalt` plaintext
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` encryptionAlgorithm
      `Prelude.hashWithSalt` grantTokens
      `Prelude.hashWithSalt` encryptionContext

instance Prelude.NFData Encrypt where
  rnf Encrypt' {..} =
    Prelude.rnf encryptionContext
      `Prelude.seq` Prelude.rnf plaintext
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf encryptionAlgorithm
      `Prelude.seq` Prelude.rnf grantTokens

instance Core.ToHeaders Encrypt where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("TrentService.Encrypt" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON Encrypt where
  toJSON Encrypt' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EncryptionContext" Core..=)
              Prelude.<$> encryptionContext,
            ("GrantTokens" Core..=) Prelude.<$> grantTokens,
            ("EncryptionAlgorithm" Core..=)
              Prelude.<$> encryptionAlgorithm,
            Prelude.Just ("KeyId" Core..= keyId),
            Prelude.Just ("Plaintext" Core..= plaintext)
          ]
      )

instance Core.ToPath Encrypt where
  toPath = Prelude.const "/"

instance Core.ToQuery Encrypt where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEncryptResponse' smart constructor.
data EncryptResponse = EncryptResponse'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the KMS key that was used to encrypt the plaintext.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The encryption algorithm that was used to encrypt the plaintext.
    encryptionAlgorithm :: Prelude.Maybe EncryptionAlgorithmSpec,
    -- | The encrypted plaintext. When you use the HTTP API or the Amazon Web
    -- Services CLI, the value is Base64-encoded. Otherwise, it is not
    -- Base64-encoded.
    ciphertextBlob :: Prelude.Maybe Core.Base64,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'encryptResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the KMS key that was used to encrypt the plaintext.
--
-- 'encryptionAlgorithm', 'encryptResponse_encryptionAlgorithm' - The encryption algorithm that was used to encrypt the plaintext.
--
-- 'ciphertextBlob', 'encryptResponse_ciphertextBlob' - The encrypted plaintext. When you use the HTTP API or the Amazon Web
-- Services CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'httpStatus', 'encryptResponse_httpStatus' - The response's http status code.
newEncryptResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EncryptResponse
newEncryptResponse pHttpStatus_ =
  EncryptResponse'
    { keyId = Prelude.Nothing,
      encryptionAlgorithm = Prelude.Nothing,
      ciphertextBlob = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the KMS key that was used to encrypt the plaintext.
encryptResponse_keyId :: Lens.Lens' EncryptResponse (Prelude.Maybe Prelude.Text)
encryptResponse_keyId = Lens.lens (\EncryptResponse' {keyId} -> keyId) (\s@EncryptResponse' {} a -> s {keyId = a} :: EncryptResponse)

-- | The encryption algorithm that was used to encrypt the plaintext.
encryptResponse_encryptionAlgorithm :: Lens.Lens' EncryptResponse (Prelude.Maybe EncryptionAlgorithmSpec)
encryptResponse_encryptionAlgorithm = Lens.lens (\EncryptResponse' {encryptionAlgorithm} -> encryptionAlgorithm) (\s@EncryptResponse' {} a -> s {encryptionAlgorithm = a} :: EncryptResponse)

-- | The encrypted plaintext. When you use the HTTP API or the Amazon Web
-- Services CLI, the value is Base64-encoded. Otherwise, it is not
-- Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
encryptResponse_ciphertextBlob :: Lens.Lens' EncryptResponse (Prelude.Maybe Prelude.ByteString)
encryptResponse_ciphertextBlob = Lens.lens (\EncryptResponse' {ciphertextBlob} -> ciphertextBlob) (\s@EncryptResponse' {} a -> s {ciphertextBlob = a} :: EncryptResponse) Prelude.. Lens.mapping Core._Base64

-- | The response's http status code.
encryptResponse_httpStatus :: Lens.Lens' EncryptResponse Prelude.Int
encryptResponse_httpStatus = Lens.lens (\EncryptResponse' {httpStatus} -> httpStatus) (\s@EncryptResponse' {} a -> s {httpStatus = a} :: EncryptResponse)

instance Prelude.NFData EncryptResponse where
  rnf EncryptResponse' {..} =
    Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf ciphertextBlob
      `Prelude.seq` Prelude.rnf encryptionAlgorithm
