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
-- Module      : Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateRenewalSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateRenewalSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateDomainValidationOption

-- | Contains information about the Certificate Manager managed renewal for
-- an @AMAZON_ISSUED@ certificate.
--
-- /See:/ 'newAwsCertificateManagerCertificateRenewalSummary' smart constructor.
data AwsCertificateManagerCertificateRenewalSummary = AwsCertificateManagerCertificateRenewalSummary'
  { -- | The status of the Certificate Manager managed renewal of the
    -- certificate.
    --
    -- Valid values: @PENDING_AUTO_RENEWAL@ | @PENDING_VALIDATION@ | @SUCCESS@
    -- | @FAILED@
    renewalStatus :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the renewal summary was last updated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    updatedAt :: Prelude.Maybe Prelude.Text,
    -- | Information about the validation of each domain name in the certificate,
    -- as it pertains to Certificate Manager managed renewal. Provided only
    -- when the certificate type is @AMAZON_ISSUED@.
    domainValidationOptions :: Prelude.Maybe [AwsCertificateManagerCertificateDomainValidationOption],
    -- | The reason that a renewal request was unsuccessful.
    --
    -- Valid values: @NO_AVAILABLE_CONTACTS@ |
    -- @ADDITIONAL_VERIFICATION_REQUIRED@ | @DOMAIN_NOT_ALLOWED@ |
    -- @INVALID_PUBLIC_DOMAIN@ | @DOMAIN_VALIDATION_DENIED@ | @CAA_ERROR@ |
    -- @PCA_LIMIT_EXCEEDED@ | @PCA_INVALID_ARN@ | @PCA_INVALID_STATE@ |
    -- @PCA_REQUEST_FAILED@ | @PCA_NAME_CONSTRAINTS_VALIDATION@ |
    -- @PCA_RESOURCE_NOT_FOUND@ | @PCA_INVALID_ARGS@ | @PCA_INVALID_DURATION@ |
    -- @PCA_ACCESS_DENIED@ | @SLR_NOT_FOUND@ | @OTHER@
    renewalStatusReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCertificateManagerCertificateRenewalSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'renewalStatus', 'awsCertificateManagerCertificateRenewalSummary_renewalStatus' - The status of the Certificate Manager managed renewal of the
-- certificate.
--
-- Valid values: @PENDING_AUTO_RENEWAL@ | @PENDING_VALIDATION@ | @SUCCESS@
-- | @FAILED@
--
-- 'updatedAt', 'awsCertificateManagerCertificateRenewalSummary_updatedAt' - Indicates when the renewal summary was last updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'domainValidationOptions', 'awsCertificateManagerCertificateRenewalSummary_domainValidationOptions' - Information about the validation of each domain name in the certificate,
-- as it pertains to Certificate Manager managed renewal. Provided only
-- when the certificate type is @AMAZON_ISSUED@.
--
-- 'renewalStatusReason', 'awsCertificateManagerCertificateRenewalSummary_renewalStatusReason' - The reason that a renewal request was unsuccessful.
--
-- Valid values: @NO_AVAILABLE_CONTACTS@ |
-- @ADDITIONAL_VERIFICATION_REQUIRED@ | @DOMAIN_NOT_ALLOWED@ |
-- @INVALID_PUBLIC_DOMAIN@ | @DOMAIN_VALIDATION_DENIED@ | @CAA_ERROR@ |
-- @PCA_LIMIT_EXCEEDED@ | @PCA_INVALID_ARN@ | @PCA_INVALID_STATE@ |
-- @PCA_REQUEST_FAILED@ | @PCA_NAME_CONSTRAINTS_VALIDATION@ |
-- @PCA_RESOURCE_NOT_FOUND@ | @PCA_INVALID_ARGS@ | @PCA_INVALID_DURATION@ |
-- @PCA_ACCESS_DENIED@ | @SLR_NOT_FOUND@ | @OTHER@
newAwsCertificateManagerCertificateRenewalSummary ::
  AwsCertificateManagerCertificateRenewalSummary
newAwsCertificateManagerCertificateRenewalSummary =
  AwsCertificateManagerCertificateRenewalSummary'
    { renewalStatus =
        Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      domainValidationOptions =
        Prelude.Nothing,
      renewalStatusReason =
        Prelude.Nothing
    }

-- | The status of the Certificate Manager managed renewal of the
-- certificate.
--
-- Valid values: @PENDING_AUTO_RENEWAL@ | @PENDING_VALIDATION@ | @SUCCESS@
-- | @FAILED@
awsCertificateManagerCertificateRenewalSummary_renewalStatus :: Lens.Lens' AwsCertificateManagerCertificateRenewalSummary (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateRenewalSummary_renewalStatus = Lens.lens (\AwsCertificateManagerCertificateRenewalSummary' {renewalStatus} -> renewalStatus) (\s@AwsCertificateManagerCertificateRenewalSummary' {} a -> s {renewalStatus = a} :: AwsCertificateManagerCertificateRenewalSummary)

-- | Indicates when the renewal summary was last updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsCertificateManagerCertificateRenewalSummary_updatedAt :: Lens.Lens' AwsCertificateManagerCertificateRenewalSummary (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateRenewalSummary_updatedAt = Lens.lens (\AwsCertificateManagerCertificateRenewalSummary' {updatedAt} -> updatedAt) (\s@AwsCertificateManagerCertificateRenewalSummary' {} a -> s {updatedAt = a} :: AwsCertificateManagerCertificateRenewalSummary)

-- | Information about the validation of each domain name in the certificate,
-- as it pertains to Certificate Manager managed renewal. Provided only
-- when the certificate type is @AMAZON_ISSUED@.
awsCertificateManagerCertificateRenewalSummary_domainValidationOptions :: Lens.Lens' AwsCertificateManagerCertificateRenewalSummary (Prelude.Maybe [AwsCertificateManagerCertificateDomainValidationOption])
awsCertificateManagerCertificateRenewalSummary_domainValidationOptions = Lens.lens (\AwsCertificateManagerCertificateRenewalSummary' {domainValidationOptions} -> domainValidationOptions) (\s@AwsCertificateManagerCertificateRenewalSummary' {} a -> s {domainValidationOptions = a} :: AwsCertificateManagerCertificateRenewalSummary) Prelude.. Lens.mapping Lens.coerced

-- | The reason that a renewal request was unsuccessful.
--
-- Valid values: @NO_AVAILABLE_CONTACTS@ |
-- @ADDITIONAL_VERIFICATION_REQUIRED@ | @DOMAIN_NOT_ALLOWED@ |
-- @INVALID_PUBLIC_DOMAIN@ | @DOMAIN_VALIDATION_DENIED@ | @CAA_ERROR@ |
-- @PCA_LIMIT_EXCEEDED@ | @PCA_INVALID_ARN@ | @PCA_INVALID_STATE@ |
-- @PCA_REQUEST_FAILED@ | @PCA_NAME_CONSTRAINTS_VALIDATION@ |
-- @PCA_RESOURCE_NOT_FOUND@ | @PCA_INVALID_ARGS@ | @PCA_INVALID_DURATION@ |
-- @PCA_ACCESS_DENIED@ | @SLR_NOT_FOUND@ | @OTHER@
awsCertificateManagerCertificateRenewalSummary_renewalStatusReason :: Lens.Lens' AwsCertificateManagerCertificateRenewalSummary (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateRenewalSummary_renewalStatusReason = Lens.lens (\AwsCertificateManagerCertificateRenewalSummary' {renewalStatusReason} -> renewalStatusReason) (\s@AwsCertificateManagerCertificateRenewalSummary' {} a -> s {renewalStatusReason = a} :: AwsCertificateManagerCertificateRenewalSummary)

instance
  Core.FromJSON
    AwsCertificateManagerCertificateRenewalSummary
  where
  parseJSON =
    Core.withObject
      "AwsCertificateManagerCertificateRenewalSummary"
      ( \x ->
          AwsCertificateManagerCertificateRenewalSummary'
            Prelude.<$> (x Core..:? "RenewalStatus")
              Prelude.<*> (x Core..:? "UpdatedAt")
              Prelude.<*> ( x Core..:? "DomainValidationOptions"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> (x Core..:? "RenewalStatusReason")
      )

instance
  Prelude.Hashable
    AwsCertificateManagerCertificateRenewalSummary
  where
  hashWithSalt
    salt'
    AwsCertificateManagerCertificateRenewalSummary' {..} =
      salt' `Prelude.hashWithSalt` renewalStatusReason
        `Prelude.hashWithSalt` domainValidationOptions
        `Prelude.hashWithSalt` updatedAt
        `Prelude.hashWithSalt` renewalStatus

instance
  Prelude.NFData
    AwsCertificateManagerCertificateRenewalSummary
  where
  rnf
    AwsCertificateManagerCertificateRenewalSummary' {..} =
      Prelude.rnf renewalStatus
        `Prelude.seq` Prelude.rnf renewalStatusReason
        `Prelude.seq` Prelude.rnf domainValidationOptions
        `Prelude.seq` Prelude.rnf updatedAt

instance
  Core.ToJSON
    AwsCertificateManagerCertificateRenewalSummary
  where
  toJSON
    AwsCertificateManagerCertificateRenewalSummary' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("RenewalStatus" Core..=) Prelude.<$> renewalStatus,
              ("UpdatedAt" Core..=) Prelude.<$> updatedAt,
              ("DomainValidationOptions" Core..=)
                Prelude.<$> domainValidationOptions,
              ("RenewalStatusReason" Core..=)
                Prelude.<$> renewalStatusReason
            ]
        )
